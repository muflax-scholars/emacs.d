;;; prompt-minor-mode.el ---

(require 'ace-jump-mode)
(require 'cl-lib)
(require 'fold-dwim)
(require 'hideshow)
(require 'rx)
(require 'ample-regexps)
(require 's)

(defvar prompt-minor-mode-map (make-keymap))
(define-minor-mode prompt-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  nil " P" prompt-minor-mode-map

  (set (make-local-variable 'prompt/corpus-file-dir)
       prompt/corpus-file-dir))

(defcustom prompt/corpus-file-dir
  (expand-file-name "~/stuff/german/predicates")
  "corpus directory to use" :group 'notes-mode)

;; cleaning

(defun prompt/clean-up-dictionary ()
  (interactive)

  (beginning-of-line)

  (let (key
        before
        after
        (regexp "^\t[^?!<>+\n]"))
    (catch 'done
      (setq before (how-many regexp))

      (while t
        (re-search-forward regexp)
        (beginning-of-line-text)
        (recenter)
        (message "decision time!")

        (setq key (read-key "?"))
        (case key
          (114                          ; r
           (kill-whole-line)
           (undo-boundary))
          (110                          ; n
           (uncomment-region (point-at-bol) (point-at-eol)))
          (116                          ; t
           (forward-line -1)
           (re-search-backward regexp))
          (100                          ; d
           (undo-tree-undo))
          (t
           (throw 'done nil)))))

    (setq after (how-many regexp))
    (message "done (%d left, %d done)"
             after
             (- before after))))

(defun prompt/fill-in-nouns ()
  (interactive)
  (let (key
        noun
        head
        (case-fold-search nil)
        (regexp "^\\[ \\([A-ZÄÖÜ]\\sw+\\) / \\([A-ZÄÖÜ]\\sw+\\)$"))
    (catch 'done
      (while t
        (re-search-forward regexp)
        (message "decision time!")
        (recenter)

        (setq noun (match-string 1))
        (setq head (match-string 2))

        (setq key (read-key "?"))
        (case key
          (114 ; r
           (end-of-line)
           (newline-and-indent)
           (newline-and-indent)
           (insert (format "? \n\t! %s\n\t! <%s>\n" noun head))
           (undo-boundary)
           ;; (forward-line -3)
           ;; (end-of-line)
           ;; (throw 'done nil)
           )
          (110 ; n
           )
          (t
           (throw 'done nil)))))))

(defun prompt/auto-fill-in-nouns ()
  (interactive)
  (let (key
        noun
        head
        endpos
        (regexp "^\\[ \\([A-ZÄÖÜ]\\sw+\\) / \\([A-ZÄÖÜ]\\sw+\\)$")
        (case-fold-search nil)
        )
    (catch 'done
      (while t
        (re-search-forward regexp)

        (setq noun (match-string 1))
        (setq head (match-string 2))

        ;; do we need to convert the current noun?
        (save-excursion
          (re-search-forward "^\\]")
          (setq endpos (point)))
        (if (re-search-forward "^\t! <\\sw+>" endpos t)
            (message "already converted")
          (progn
            (end-of-line)
            (newline-and-indent)
            (newline-and-indent)
            (insert (format "? \n\t! %s\n\t! <%s>\n" noun head))
            (undo-boundary)))
        ))))

(defun prompt/next-empty-prompt ()
  (interactive)

  (let ((regexp "^\t+[?][ \t]*$"))
    (re-search-forward regexp)
    (recenter)))

(defun prompt/convert-dict-entry ()
  (interactive)

  (beginning-of-line)

  (let (entry
        translation
        curpos
        (single-regexp	"^[ \t]+[-][ \t]*\\([^\n]+\\)$")
        (double-regexp	"^[ \t]+{\\([^}\n]+\\)}\\([^{}\n]*\\)$")
        changed
        )
    (when (looking-at single-regexp)
      (setq changed t)
      (setq translation (s-trim (match-string 1)))

      (kill-line)
      (setq curpos (point))

      (insert "? " translation)	(newline)
      (insert "! ")            	(newline)
      (indent-region curpos (point)))

    (when (looking-at double-regexp)
      (setq changed t)

      (setq entry      	(s-trim (match-string 1)))
      (setq translation	(s-trim (match-string 2)))

      (kill-line)
      (setq curpos (point))

      (insert "? " translation)	(newline)
      (insert "! " entry)      	(newline)
      (indent-region curpos (point)))

    (goto-char curpos)
    (beginning-of-line-text)

    changed))

(defun prompt/convert-all-dict-entry ()
  (interactive)

  (let (firstpos)
    (save-excursion
      (beginning-of-line)

      ;; limit to current word
      (prompt/beginning-of-bracket)
      (forward-line)

      (while (not (looking-at-p "^\\]"))
        (when (and (prompt/convert-dict-entry)
                   (not firstpos))
          (setq firstpos (point)))
        (forward-line)))

    ;; go to first converted example, if possible
    (when firstpos
      (goto-char firstpos)
      (beginning-of-line-text))

    (recenter-top-bottom)
    ))

(defun prompt/next-simple-noun ()
  (interactive)

  (let (key
        begpos
        endpos
        noun
        head
        lines
        (noun-re	"^\\[ \\([A-ZÄÖÜ]\\sw+\\) / \\([A-ZÄÖÜ]\\sw+\\)$")
        (dict-re	"^[ \t]+\\({.*}\\|-\\)")
        (case-fold-search nil)
        )
    (catch 'done
      (while t
        (re-search-forward noun-re)

        (setq noun (match-string 1))
        (setq head (match-string 2))

        (setq begpos (point))
        (save-excursion
          (re-search-forward "^\\]")
          (setq endpos (point)))

        (message "%d for %s" (how-many dict-re begpos endpos) noun)

        (setq lines (how-many dict-re begpos endpos))

        (when (and (> lines 0) (< lines 3))
          (re-search-forward dict-re)
          (recenter)
          (throw 'done nil))

        (goto-char endpos)))))

(defun prompt/next-simple-dict-line ()
  (interactive)

  (let (key
        (dict-re 	"^[ \t]+\\({.*}\\|-\\)")
        (one-word	"^[ \t]+{\\sw+}[^\n{}]*$")
        (no-prep 	"^[ \t]+[^\n{}]+$")
        (case-fold-search nil)
        )
    (catch 'done
      (while t
        (forward-line)
        (re-search-forward dict-re)

        (beginning-of-line)
        (when (or (looking-at-p one-word)
                  (looking-at-p no-prep))
          (recenter)
          (throw 'done nil))))))


;; folding

(defun prompt/fold-toggle-words (regexp)
  (let (
        (headline        	(format "^\\[ %s\\b" regexp))
        (case-fold-search	nil)
        )
    (goto-char (point-min))
    (while (re-search-forward headline nil t)
      (fold-dwim-toggle))
    (goto-char (point-min))
    (re-search-forward headline nil t)))

(defun prompt/fold-invert ()
  (interactive)

  (let ((headline "^\\[ \\sw+"))
    (goto-char (point-min))
    (while (re-search-forward headline nil t)
      (fold-dwim-toggle))
    (goto-char (point-min))
    (re-search-forward headline nil t)))

(defun prompt/fold-hide-all ()
  (interactive)

  (goto-char (point-min))
  (fold-dwim-hide-all))

(defun prompt/fold-show-all ()
  (interactive)

  (goto-char (point-min))
  (fold-dwim-show-all))

(defun prompt/fold-show-adjectives (&optional hideall)
  (interactive)

  (unless hideall
    (prompt/fold-hide-all))
  (prompt/fold-toggle-words "\\sw+ / \\(alt\\|klein\\|rot\\|nah\\)"))

(defun prompt/fold-show-verbs (&optional hideall)
  (interactive)

  (unless hideall
    (prompt/fold-hide-all))
  (prompt/fold-toggle-words "\\(\\sw+\\(ern\\|eln\\|en\\)\\|\\sw*tun\\|\\sw*sein\\) / \\(\\sw+\\(ern\\|eln\\|en\\)\\|tun\\|sein\\)"))

(defun prompt/fold-show-nouns (&optional hideall)
  (interactive)

  (unless hideall
    (prompt/fold-hide-all))
  (prompt/fold-toggle-words "[A-ZÄÖÜ][-a-zäöüßA-ZÄÖÜ]* / \\([A-ZÄÖÜ]\\sw*\\|plural\\)"))

(defun prompt/fold-rest ()
  (interactive)

  (prompt/fold-show-all)         	(sit-for 0.01)
  (prompt/fold-show-adjectives t)	(sit-for 0.01)
  (prompt/fold-show-verbs t)     	(sit-for 0.01)
  (prompt/fold-show-nouns t)

  (goto-char (point-min)))

(defun prompt/fold-plus-lines ()
  (interactive)

  (let (beg-fold
        end-fold
        end-loop
        seen-blank-p
        not-blank-p
        (line-regexp     	"^[ \t]*[^ \t\n]")
        (plus-line-regexp	"^[ \t]+[+]"))

    ;; end of example bracket
    (prompt/end-of-bracket)
    (setq end-loop (point))

    ;; start and collect all plus blocks
    (prompt/beginning-of-bracket)
    (forward-line)

    (while (< (point) end-loop)
      (setq not-blank-p (looking-at-p line-regexp))

      (when (and not-blank-p (not beg-fold))
        (setq beg-fold (point)))

      (when (and (not not-blank-p) beg-fold)
        (setq seen-blank-p t))

      (when (looking-at-p plus-line-regexp)
        ;; full block available for folding?
        (when (and beg-fold end-fold)
          (save-excursion
            (hs-hide-comment-region beg-fold end-fold)))

        (setq beg-fold    	(point))
        (setq end-fold    	nil)
        (setq seen-blank-p	nil))

      ;; make sure only to fold up to the last block, but not that block
      (when (and not-blank-p seen-blank-p)
        (save-excursion
          (forward-line -1)
          (end-of-line)
          (setq end-fold (point)))
        (setq seen-blank-p nil))

      (forward-line))

    ;; fold potential last block
    (when (and beg-fold end-fold)
      (hs-hide-comment-region beg-fold end-fold))

    ;; move to first block
    (prompt/beginning-of-bracket)
    (forward-line)
    (beginning-of-line-text)
    ))


;; bunf

(defun prompt/bunf-current-line ()
  (interactive)

  (let ((current-line	(line-number-at-pos))
        (current-file	(buffer-file-name))
        (bunf-client 	(expand-file-name "~/bunf/bunf_client.rb"))
        )

    (when (call-process bunf-client nil nil nil
                        "-l" (int-to-string current-line))
      (message "bunf'd!"))))

;; navigation

(defmacro prompt/for-each-headline (&rest body)
  `(save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^\\[ \\sw+" nil t)
       (save-excursion
         (beginning-of-line)
         ,@body))))

(defun prompt/next-block-regexp (line-regexp &optional n)
  (unless n (setq n 1))

  (loop repeat n do
        (progn
          ;; move to a matching block
          (beginning-of-line)
          (while (and (looking-at-p line-regexp)
                      (not (eobp)))
            (forward-line))

          ;; go to next block, if possible
          (while (and (not (looking-at-p line-regexp))
                      (not (eobp)))
            (forward-line))))

  (beginning-of-line-text)

  (when (< (lines-below-point)
           corpus/window-margin)
    (recenter (- corpus/window-margin))))

(defun prompt/prev-block-regexp (line-regexp &optional n)
  (unless n (setq n 1))

  (loop repeat n do
        (progn
          ;; move to a matching block
          (beginning-of-line)
          (while (and (looking-at-p line-regexp)
                      (not (bobp)))
            (forward-line -1))

          ;; go to next block, if possible
          (while (and (not (looking-at-p line-regexp))
                      (not (bobp)))
            (forward-line -1))

          ;; go to beginning of block, if we are in one
          (while (and (looking-at-p line-regexp)
                      (not (bobp)))
            (forward-line -1))
          (forward-line)))

  (beginning-of-line-text)

  (when (< (lines-above-point)
           corpus/window-margin)
    (recenter corpus/window-margin)))

(defun prompt/next-example-block (&optional n)
  (interactive)
  (prompt/next-block-regexp "^[ \t]+[?!<>#*+]" n))

(defun prompt/prev-example-block (&optional n)
  (interactive)
  (prompt/prev-block-regexp "^[ \t]+[?!<>#*+]" n))

(defun prompt/next-example-grab-bracket ()
  (interactive)

  (beginning-of-line)
  (while (and (not (looking-at-p "^[ \t]*[\\[{]"))
              (not (eobp)))
    (forward-line))

  (prompt/next-example-block)
  (recenter 3))

(defun prompt/prev-example-grab-bracket ()
  (interactive)

  (beginning-of-line)
  (while (and (not (looking-at-p "^[ \t]*[[{]"))
              (not (bobp)))
    (forward-line -1))
  (prompt/prev-example-block)

  (while (and (not (looking-at-p "^[ \t]*[[{]"))
              (not (bobp)))
    (forward-line -1))

  (prompt/next-example-block)
  (recenter 3))

(defun prompt/beginning-of-example-grab-bracket ()
  (interactive)

  (prompt/beginning-of-bracket)
  (prompt/next-example-block)
  (recenter-top-bottom))

(defun prompt/end-of-example-grab-bracket ()
  (interactive)

  (prompt/end-of-bracket)
  (prompt/prev-example-block)

  (when (< (lines-below-point)
           corpus/window-margin)
    (recenter (- corpus/window-margin))))

(defun prompt/beginning-of-bracket ()
  (interactive)

  (beginning-of-line)
  (while (not (looking-at-p "^\\["))
    (forward-line -1)))

(defun prompt/end-of-bracket ()
  (interactive)

  (beginning-of-line)
  (while (not (looking-at-p "^\\]"))
    (forward-line)))

(defun prompt/next-bracket ()
  (interactive)

  (prompt/end-of-bracket)

  (beginning-of-line)
  (while (not (looking-at-p "^\\["))
    (forward-line))

  (recenter 0))

(defun prompt/prev-bracket ()
  (interactive)

  (prompt/beginning-of-bracket)

  (forward-line -1)
  (while (not (looking-at-p "^\\["))
    (forward-line -1))

  (recenter 0))

(defun prompt/beginning-of-plus-block ()
  (interactive)

  (let ((beg-regexp  "^\\([ \t]+[+]\\|\\[\\)"))

    (beginning-of-line)

    (while (not (looking-at-p beg-regexp))
      (forward-line -1))

    (beginning-of-line-text)))

(defun prompt/end-of-plus-block ()
  (interactive)

  (let ((line-regexp	"^[ \t]*[^ \t\n]")
        (plus-regexp	"^[ \t]+[+]")
        (end-regexp 	"^\\([ \t]+[+]\\|\\]\\)")
        beg
        )

    (beginning-of-line)
    (when (looking-at-p plus-regexp)
      (forward-line))

    (setq beg (point))

    (while (not (looking-at-p end-regexp))
      (forward-line))

    (forward-line -1)

    (while (and (> (point) beg)
                (not (looking-at-p line-regexp)))
      (forward-line -1))

    (beginning-of-line-text)))

(defun prompt/next-block (&optional n)
  (interactive)
  (prompt/next-block-regexp "^[ \t]+[^ \t\n]" n))

(defun prompt/prev-block (&optional n)
  (interactive)
  (prompt/prev-block-regexp "^[ \t]+[^ \t\n]" n))

(defun prompt/get-plus-positions ()
  (let (positions
        end
        last-plus-p
        (plus-line-regexp "^[ \t]+[+]"))
    (save-excursion
      (save-excursion
        (prompt/beginning-of-bracket)
        (setq end (point))
        (forward-line)
        (setq positions (cons (point) positions)))

      (prompt/end-of-bracket)
      (forward-line -1)

      (setq positions (cons (point) positions))

      (while (> (point) end)
        (if (looking-at-p plus-line-regexp)
            (progn
              (unless last-plus-p
                (setq positions (cons (point) positions)))
              (setq last-plus-p t))
          (setq last-plus-p nil))
        (forward-line -1)))

    (loop for pos in (sort (delete-dups positions) '<)
          collect (make-aj-position
                   :offset pos
                   :visual-area (make-aj-visual-area
                                 :buffer (current-buffer)
                                 :window (selected-window)
                                 :frame  (selected-frame))))))

(defun prompt/select-plus-line ()
  (interactive)

  (when ace-jump-search-tree
    (ace-jump-done))

  (prompt/fold-plus-lines)
  (prompt/beginning-of-bracket)

  (when (> (lines-above-point) 0)
    (recenter 0))

  ;; jump to plus line
  (let ((candidate-list (prompt/get-plus-positions))
        jump-char
        index
        node)
    (cond
     ;; cannot find any one
     ((null candidate-list)
      (error "no plus lines found"))
     ;; we only find one, so move to it directly
     ((eq (rest candidate-list) nil)
      (ace-jump-jump-to (first candidate-list)))
     ;; more than one, we need to enter AceJump mode
     (t
      ;; construct search tree and populate overlay into tree
      (setq ace-jump-search-tree
            (ace-jump-tree-breadth-first-construct (length candidate-list)
                                                   (length ace-jump-mode-move-keys)))
      (ace-jump-populate-overlay-to-search-tree ace-jump-search-tree
                                                candidate-list)
      (ace-jump-update-overlay-in-search-tree ace-jump-search-tree
                                              ace-jump-mode-move-keys)

      (setq jump-char (read-char "jump to?"))

      (cond
       ;; ((= jump-char 32)
       ;;  ;; next page
       ;;  )

       ((member jump-char ace-jump-mode-move-keys)
        (setq index (let ((ret (cl-position jump-char
                                            ace-jump-mode-move-keys)))
                      (if ret ret (length ace-jump-mode-move-keys))))
        (setq node (nth index (cdr ace-jump-search-tree)))

        (when node
          (let ((aj-data (overlay-get (cdr node) 'aj-data)))
            (ace-jump-push-mark)
            (run-hooks 'ace-jump-mode-before-jump-hook)
            (ace-jump-jump-to aj-data)))

        (ace-jump-done))

       (t
        ;; abort
        (ace-jump-done)))
      ))))

;; editing

(defun prompt/size-of-current-slot ()
  (let ((beg (save-excursion
               (or (progn (re-search-backward "\\[" (point-at-bol) t)
                          (forward-char 1) (point))
                   (beginning-of-thing 'symbol))))
        (end (save-excursion
               (or (progn (re-search-forward "\\]" (point-at-eol) t)
                          (forward-char -1) (point))
                   (end-of-thing 'symbol)))))

    (list beg end)))

(defun prompt/change-plus-slot ()
  (interactive)
  ;; put in new word
  (apply 'yas-expand-snippet "slot"
         (prompt/size-of-current-slot)))

(defun prompt/delete-plus-slot ()
  (interactive)
  ;; clear position
  (apply 'delete-region
         (prompt/size-of-current-slot)))

(defun prompt/get-used-slots ()
  (let ((slots      	(make-hash-table :test 'equal))
        (plus-regexp	"^[\t]*\\+")
        (slot-regexp	"\\[\\([^]]+\\)\\]")
        slot
        slot-list
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward plus-regexp nil t)
        (while (re-search-forward slot-regexp (point-at-eol) t)
          (setq slot (match-string-no-properties 1))
          (puthash slot
                   (1+ (gethash slot slots 0))
                   slots))))

    (maphash (lambda (word count)
               (setq slot-list
                     (cons (list count word) slot-list)))
             slots)
    (sort slot-list (lambda (a b)
                      (< (first a) (first b))))
    (setq slot-list
          (mapcar (lambda (el) (second el))
                  slot-list))

    slot-list))

(define-arx prompt/slot-rx
  '((slot   	(and "[" (+ (not (any "]"))) "]"))
    (article	(or "der" "des" "den" "dem" "einen" "einem" "eines" "ein"))
    (prep   	(or "an" "auf" "aus" "bei" "durch"
                  "für" "gegen" "hinter" "in" "mit"
                  "nach" "ohne" "um" "unter" "von"
                  "vor" "wegen" "zu" "zwischen" "über"))
    (clause 	(seq (? "," (+ blank)) ; might have a leading whitespace
                   (or "dass" "m0" "mc" "wc" "als ob"
                       (seq "zu_" (+ (not blank))))))
    ))

(defvar prompt/slot-group
  (prompt/slot-rx (or (* prep  	(+ blank))
                      (* clause	(+ blank)))
                  (* article   	(+ blank))
                  (slot)))

(defun prompt/make-slot-optional ()
  (interactive)

  (let ((orig-point	(point))
        (beg       	nil)
        (end       	(point-at-bol)))

    (save-excursion
      (beginning-of-line)
      (while (and (< end orig-point)
                  (re-search-forward prompt/slot-group (point-at-eol) t))
        (setq beg (match-beginning 0)
              end (match-end 0)))

      ;; found a match?
      (when (and beg end)
        ;; wrap with parens (backwards so the positions stay correct)
        (goto-char end)
        (insert ")")
        (goto-char beg)
        (insert "(")))))

(defun prompt/highlight-un-optional-plus-lines ()
  (interactive)

  (highlight-regexp
   (rx bol (* blank) "+"
       (* (not (any "(" "\n")))
       "["
       (* (not (any "(" "\n")))
       eol)))

(defun prompt/size-of-current-block ()
  (let (beg
        end
        (line-regexp	"^[ \t]+[?!<>#*]"))

    ;; move to a matching block
    (beginning-of-line)
    (while (not (looking-at-p line-regexp))
      (forward-line))

    ;; find beginning of block
    (setq beg
          (catch 'done
            (while t
              (beginning-of-line)
              (if (looking-at-p line-regexp)
                  (forward-line -1)
                (progn
                  (forward-line)
                  (throw 'done (point)))))))

    ;; find end of block
    (setq end
          (catch 'done
            (while t
              (beginning-of-line)
              (if (looking-at-p line-regexp)
                  (forward-line)
                (progn
                  (forward-line -1)
                  (end-of-line)
                  (throw 'done (point)))))))

    (list beg end)))

(defun prompt/grab-block-string (beg end)
  (replace-regexp-in-string
   "^\\s-+" ""
   (buffer-substring-no-properties beg end)))

(defun prompt/beginning-of-current-block ()
  (let ((line-regexp	"^[ \t]+[?!<>#*]"))

    (catch 'done
      (while t
        (beginning-of-line)
        (if (looking-at-p line-regexp)
            (forward-line -1)
          (progn
            (forward-line)
            (throw 'done nil)))))
    (beginning-of-line-text)))

(defun prompt/copy-example-block ()
  (interactive)
  (let* ((block-pos    	(prompt/size-of-current-block))
         (example-block	(apply 'prompt/grab-block-string block-pos)))

    (newline-and-indent)
    (newline-and-indent)
    (insert example-block)
    (indent-region (second block-pos) (point))

    ;; go to beginning of block
    (prompt/beginning-of-current-block)))

(defun prompt/kill-words (regexp)
  (let (beg
        selection
        (headline (format "^\\[ %s\\b" regexp))
        (spew (make-progress-reporter "Killing everything..."
                                      (point-min) (point-max)))
        (case-fold-search nil))

    (goto-char (point-min))
    (while (re-search-forward headline nil t)
      (setq beg (point-at-bol))
      (re-search-forward "^\\]")
      (forward-line 2)

      (setq selection (append selection
                              `(,(buffer-substring-no-properties beg (point)))))
      (delete-region beg (point))

      (progress-reporter-update spew (point)))

    (kill-new (s-join "" selection))

    (progress-reporter-done spew)
    (goto-char (point-min))))

(defun prompt/kill-all-adjectives ()
  (interactive)
  (prompt/kill-words "\\sw+ / \\(alt\\|klein\\|rot\\|nah\\)"))

(defun prompt/kill-all-verbs ()
  (interactive)
  (prompt/kill-words "\\(\\sw+\\(ern\\|eln\\|en\\)\\|\\sw*tun\\|\\sw*sein\\) / \\(\\sw+\\(ern\\|eln\\|en\\)\\|tun\\|sein\\)"))

(defun prompt/kill-all-nouns ()
  (interactive)
  (prompt/kill-words "[A-ZÄÖÜ][-a-zäöüßA-ZÄÖÜ]* / \\([A-ZÄÖÜ]\\sw*\\|plural\\)"))

(defun prompt/add-example-to-plus-line ()
  (interactive)

  ;; position cursor in templates file
  (other-window 1)
  (prompt/select-plus-line)
  (prompt/end-of-plus-block)
  (other-window 1)

  (corpus/copy-example-block-over)

  ;; refold? unsure; let's not for now
  ;; (other-window 1)
  ;; (fold-plus-lines)
  ;; (other-window 1)

  )

(defun prompt/kill-example-block ()
  (interactive)

  (let (begpos
        endpos
        (line-regexp 	"^[ \t]*[?!<>#*]")
        (blank-regexp	"^[ \t]*$")
        ret)

    ;; move to a matching block, if possible
    (beginning-of-line)
    (while (looking-at-p blank-regexp)
      (forward-line))

    (when (looking-at-p line-regexp)
      (setq ret t)

      ;; find beginning of block
      (save-excursion
        (while (looking-at-p line-regexp)
          (forward-line -1))
        (forward-line)
        (setq begpos (point)))

      ;; find end of block
      (save-excursion
        (while (looking-at-p line-regexp)
          (forward-line))
        (forward-line -1)
        (end-of-line)
        (setq endpos (point)))

      (kill-region begpos endpos)

      ;; get rid of whitespace
      (while (looking-at-p blank-regexp)
        (save-excursion
          (setq begpos (point))
          (forward-line)
          (setq endpos (point))
          (delete-region begpos endpos)))
      )

    (beginning-of-line-text)

    ret))

(defun prompt/select-example-block ()
  (interactive)

  (let (begpos
        endpos
        (line-regexp 	"^[ \t]*[?!<>#*]")
        (blank-regexp	"^[ \t]*$"))

    ;; move to a matching block, if possible
    (beginning-of-line)
    (while (looking-at-p blank-regexp)
      (forward-line))

    (when (looking-at-p line-regexp)

      ;; find beginning of block
      (while (looking-at-p line-regexp)
        (forward-line -1))
      (forward-line)
      (set-mark (point))

      ;; find end of block
      (while (looking-at-p line-regexp)
        (forward-line)))
    ))

(defun prompt/move-example-block-down ()
  (interactive)

  (fold-dwim-show-all)

  (let (break-p
        seen-break-p
        endpos
        (break-regexp	"^[ \t]*$\\|^[ \t]*[+]")
        (blank-regexp	"^[ \t]*$"))

    (when (prompt/kill-example-block)
      (save-excursion
        (prompt/end-of-bracket)
        (setq endpos (point)))

      (beginning-of-line)

      (catch 'done
        (while (< (point) endpos)
          (if (not seen-break-p)
              (setq break-p (looking-at-p break-regexp))
            (setq break-p (looking-at-p blank-regexp)))

          (when break-p
            (setq seen-break-p t))

          (when (and (not break-p)
                     seen-break-p)
            (throw 'done nil))

          (forward-line)))

      (save-excursion
        (yank-and-indent)
        (newline-and-indent)
        (newline-and-indent))
      )))

(defun prompt/move-example-block-up ()
  (interactive)

  (fold-dwim-show-all)

  (let (break-p
        seen-break-p
        endpos
        (break-regexp	"^[ \t]*$\\|^[ \t]*[+]")
        (blank-regexp	"^[ \t]*$"))

    (when (prompt/kill-example-block)
      (save-excursion
        (prompt/beginning-of-bracket)
        (forward-line -1)
        (setq endpos (point)))

      (forward-line -1)
      (while (looking-at-p break-regexp)
        (forward-line -1))

      (catch 'done
        (while (> (point) endpos)
          (if (not seen-break-p)
              (setq break-p (looking-at-p break-regexp))
            (setq break-p (looking-at-p blank-regexp)))

          (when break-p
            (setq seen-break-p t))

          (when (and (not break-p)
                     seen-break-p)
            (throw 'done nil))

          (forward-line -1)))

      (forward-line)

      (save-excursion
        (end-of-line)
        (newline-and-indent)
        (yank-and-indent)
        (newline-and-indent))

      (forward-line)
      )))

(provide 'prompt-minor-mode)
;;; prompt-minor-mode.el ends here
