;; Semi-port of surround.vim, based on Tim Harper's version.
;;; Code:

(require 'evil)

(defgroup surround nil
  "surround.vim for Emacs"
  :prefix "surround-"
  :group 'evil)

(defcustom surround-pairs-alist
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    (?b . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    (?t . surround-read-tag)
    (?< . surround-read-tag))
  "Association list of surround items.
Each item is of the form (TRIGGER . (LEFT . RIGHT)), all strings.
Alternatively, a function can be put in place of (LEFT . RIGHT).
This only affects inserting pairs, not deleting or changing them."
  :group 'surround
  :type '(repeat (cons (regexp :tag "Key")
                       (symbol :tag "Surround pair"))))

(defvar surround-read-tag-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map ">" 'exit-minibuffer)
    map)
  "Keymap used by `surround-read-tag'.")

(defun surround-read-tag ()
  "Read a XML tag from the minibuffer."
  (let* ((input (read-from-minibuffer "<" "" surround-read-tag-map))
         (match (string-match "\\([0-9a-z-]+\\)\\(.*?\\)[>]*$" input))
         (tag (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" (or tag "") (or rest ""))
          (format "</%s>" (or tag "")))))

(defun surround-pair (char)
  "Return the surround pair of char.
This is a cons cell (LEFT . RIGHT), both strings."
  (let ((pair (assoc-default char surround-pairs-alist)))
    (cond
     ((functionp pair)
      (funcall pair))

     ((consp pair)
      pair)

     (t
      (cons (format "%c" char) (format "%c" char))))))

(defun surround-outer-overlay (char)
  "Return outer overlay for the delimited range represented by CHAR.
This overlay includes the delimiters.
See also `surround-inner-overlay'."
  (let ((outer (lookup-key evil-outer-text-objects-map (string char))))
    (when (functionp outer)
      (setq outer (funcall outer))
      (when (evil-range-p outer)
        (setq outer (make-overlay (evil-range-beginning outer)
                                  (evil-range-end outer)
                                  nil nil t))))))

(defun surround-trim-whitespace-from-range (range &optional regexp)
  "Given an evil-range, trim whitespace around range by shrinking the range such that it neither begins nor ends with whitespace. Does not modify the buffer."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (while (looking-at regexp) (forward-char))
        (evil-set-range-beginning range (point))
        (goto-char (evil-range-end range))
        (while (looking-back regexp) (backward-char))
        (evil-set-range-end range (point))))))

(defun surround-inner-overlay (char)
  "Return inner overlay for the delimited range represented by CHAR.
This overlay excludes the delimiters.
See also `surround-outer-overlay'."
  (let ((inner (lookup-key evil-inner-text-objects-map (string char))))
    (when (functionp inner)
      (setq inner (funcall inner))
      (when (evil-range-p inner)
        (when (eq (char-syntax char) ?\()
          (surround-trim-whitespace-from-range inner "[ \t]"))
        (setq inner (make-overlay (evil-range-beginning inner)
                                  (evil-range-end inner)
                                  nil nil t))))))

(defun surround-delete (char &optional outer inner)
  "Delete the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with
the overlays OUTER and INNER, where OUTER includes the delimiters
and INNER excludes them. The intersection (i.e., difference)
between these overlays is what is deleted."
  (interactive "c")
  (cond
   ((and outer inner)
    (delete-region (overlay-start outer) (overlay-start inner))
    (delete-region (overlay-end inner) (overlay-end outer))
    (goto-char (overlay-start outer)))
   (t
    ;; no overlays specified: create them on the basis of CHAR
    ;; and delete after use
    (let* ((outer (surround-outer-overlay char))
           (inner (surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (surround-delete char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

(defun surround-delete-within (char &optional inner)
  "Like `surround-delete', but deletes the content, not the
delimiters."
  (interactive "c")
  (cond
   (inner
    (delete-region (overlay-start inner) (overlay-end inner))
    (goto-char (overlay-start inner)))
   (t
    ;; no overlays specified: create them on the basis of CHAR
    ;; and delete after use
    (let* ((inner (surround-inner-overlay char)))
      (unwind-protect
          (when inner
            (surround-delete-within char inner))
        (when inner (delete-overlay inner)))))))


(defun surround-change (char &optional outer inner)
  "Change the surrounding delimiters represented by CHAR.
Alternatively, the text to delete can be represented with the
overlays OUTER and INNER, which are passed to `surround-delete'."
  (interactive "c")
  (cond
   ((and outer inner)
    (surround-delete char outer inner)
    (surround-region (overlay-start outer)
                     (overlay-end outer)
                     nil (read-char)))
   (t
    (let* ((outer (surround-outer-overlay char))
           (inner (surround-inner-overlay char)))
      (unwind-protect
          (when (and outer inner)
            (surround-change char outer inner))
        (when outer (delete-overlay outer))
        (when inner (delete-overlay inner)))))))

;; TODO make this interactive, too
(defun surround-region (beg end type char &optional force-new-line)
  "Surround BEG and END with CHAR.

When force-new-line is true, and region type is not line, the
following: (vertical bars indicate region start/end points)

do |:thing|

Becomes this:

do {
:thing
}"

  (let* ((overlay (make-overlay beg end nil nil t))
         (pair (surround-pair char))
         (open (car pair))
         (close (cdr pair)))
    (unwind-protect
        (progn
          (goto-char (overlay-start overlay))

          (cond ((eq type 'line)
                 (insert open)
                 (indent-according-to-mode)
                 (newline-and-indent)
                 (goto-char (overlay-end overlay))
                 (insert close)
                 (indent-according-to-mode)
                 (newline))

                (force-new-line
                 (insert open)
                 (indent-according-to-mode)
                 (newline-and-indent)
                 (goto-char (overlay-end overlay))
                 (newline-and-indent)
                 (insert close))

                (t
                 (insert open)
                 (goto-char (overlay-end overlay))
                 (insert close)))
          (goto-char (overlay-start overlay)))
      (delete-overlay overlay))))

(provide 'surround)
;;; surround.el ends here

