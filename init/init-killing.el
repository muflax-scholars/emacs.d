;; killing all kinds of things

;; options
(setq kill-do-not-save-duplicates t)

;; delete all space before point up to beginning of line or non-whitespace char
(require 'hungry-delete)
;; (global-hungry-delete-mode)
(defun literal-delete-char (&optional arg)
  (interactive "P")
  (delete-char 1))
(defun literal-delete-backward-char (&optional arg)
  (interactive "P")
  (delete-char -1))

;; FIXME the mode is terribly broken, but the standard functions work well enough
;; if no region is active, act on current line
(require 'whole-line-or-region)

(defun copy-line ()
  "Copy to end of line."
  (interactive)
  (set 'this-command 'copy-to-kill)
  (save-excursion
    (set-mark (point))
    (if (= (point) (line-end-position))
        (forward-line)
      (goto-char (line-end-position)))
    (if (eq last-command 'copy-to-kill)
        (append-next-kill))
    (kill-ring-save (mark) (point))))

(defun copy-beginning-of-line ()
  "Copy to beginning of line."
  (interactive)
  (set 'this-command 'copy-to-kill)
  (save-excursion
    (set-mark (point))
    (if (= (point) (line-beginning-position))
        (backward-line)
      (goto-char (line-beginning-position)))
    (if (eq last-command 'copy-to-kill)
        (append-next-kill))
    (kill-ring-save (mark) (point))))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  ;; (call-interactively 'whole-line-or-region-yank)
  (yank)
  (call-interactively 'indent-region))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
  Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(defun kill-beginning-of-line (&optional arg)
  "Kill to the beginning of the line."
  (interactive "P")
  (kill-line (- (or arg 0))))

(defun kill-beginning-of-line-and-join-backward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
  Deletes whitespace at join."
  (interactive "P")
  (kill-beginning-of-line arg)
  (when (and (bolp) (not (eolp)))
    (delete-indentation)))

(defun blank-line ()
  "Intelligently blanks the line."
  (interactive)
  (smart-beginning-of-line)
  (kill-line))

(defun compact-blank-lines ()
  "replace multiple blank lines with a single one"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
      (replace-match "\n")
      (forward-char 1))))

(defun kill-without-append (&optional arg)
  "Kills line (region or whole) without appending it to the last kill."
  (interactive "P")
  (setq last-command nil)
  (whole-line-or-region-kill-region arg))

(defun kill-with-append (&optional arg)
  "Kills line (region or whole) and appends it to last kill."
  (interactive "P")
  (append-next-kill)
  (whole-line-or-region-kill-region arg))

(defun copy-with-append (&optional arg)
  "Copies line (region or whole) and appends it to last kill."
  (interactive "P")
  (append-next-kill)
  (whole-line-or-region-kill-ring-save arg))

;; move lines like in org-mode
(require 'move-dup)

;; expand-region to mark stuff
(require 'expand-region)
(setq expand-region-contract-fast-key	"<left>")
(setq expand-region-reset-fast-key   	"SPC")

;; notes-mode speed-up
(defun er/add-notes-mode-expansions ()
  "Adds notes-mode expansions for buffers in notes-mode"
  (set (make-local-variable 'er/try-expand-list)
       (default-value 'er/try-expand-list))
  (loop for fun in '(er/mark-email er/mark-url)
        collect (set 'er/try-expand-list
                     (remove fun er/try-expand-list))))

(er/enable-mode-expansions 'notes-mode 'er/add-notes-mode-expansions)

;; use shift to mark things
(setq shift-select-mode t)

;; normally smartparens wraps selected text, but if input is not a pair, just overwrite the text
(require 'delsel)
(delete-selection-mode 1)

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defun copy-matching-lines (regexp &optional rstart rend interactive)
  "Copy lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command."
  (interactive
   (keep-lines-read-args "Copy lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (copy-region-as-kill (or rstart (line-beginning-position))
                             (or rend (point-max))))
      (kill-buffer))))

(provide 'init-killing)
