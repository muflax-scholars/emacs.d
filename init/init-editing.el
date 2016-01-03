;; generic editing features

;; safety
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name (emacs-d "cache/autosave/")))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)
;; move files to trash instead
(setq delete-by-moving-to-trash t)

;; save location inside buffer
(require 'saveplace)

;; save minibuffer history
(require 'savehist)
(savehist-mode 1)
(setq kill-ring-max           	1000)
(setq history-length          	1000)
(setq search-ring-max         	100)
(setq regexp-search-ring-max  	100)
(setq kmacro-ring-max         	100)
(load-after 'comint           	
  (setq comint-input-ring-size	1000))

(setq save-place-file	(emacs-d "cache/saveplace"))
(setq savehist-file  	(emacs-d "cache/history"))

(setq-default save-place t)
(setq savehist-additional-variables
      '(
        compile-command
        kill-ring
        regexp-search-ring
        search-ring
        ))

;; text stuff
(setq undo-limit     	10000000)
(setq message-log-max	1000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

;; enable useful command
(put 'narrow-to-region	'disabled nil)
(put 'downcase-region 	'disabled nil)
(put 'upcase-region   	'disabled nil)

;; UTF-8 please
(setq locale-coding-system  	'utf-8) ; pretty
(set-terminal-coding-system 	'utf-8) ; pretty
(set-keyboard-coding-system 	'utf-8) ; pretty
(set-selection-coding-system	'utf-8) ; please
(prefer-coding-system       	'utf-8) ; with sugar on top

;; multiple cursors
(setq mc/list-file (emacs-d "mc-lists.el"))
(require 'multiple-cursors)
(require 'mc-extras)
(require 'mc-jump)

(defun mc/many-to-one-yank ()
  "Yanks killed lines from multiple cursors into one position. Less messy than yank-rectangle."
  (interactive)
  (with-temp-buffer
    (yank-rectangle)
    (kill-ring-save (point-min) (point-max)))
  (yank))

(defun mc/many-to-one-yank-indent ()
  "Yanks killed lines from multiple cursors into one position, and indents. See 'mc/many-to-one-yank'."
  (interactive)
  (mc/many-to-one-yank)
  (call-interactively 'indent-region))

;; when would you ever *not* want a regexp?!
(defalias 'mc/mark-all-in-region 'mc/mark-all-in-region-regexp)

;; undo-tree like in vim
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to its full path."
  (interactive "*fInsert file name: \nP")
  (cond ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert (file-relative-name filename)))))

;; make mouse more usable
(mouse-wheel-mode t)
(setq make-pointer-invisible t)
(setq mouse-yank-at-point t)

;; yet another savior
(defun jesus ()
  "Because Jesus saves."
  (interactive)
  (save-buffer))

;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
(add-hook 'notes-mode-hook 'use-tabs)

;; automatically indent on return
(require 'electric)
(electric-indent-mode 1)
(defadvice electric-indent-post-self-insert-function (around keep-trailing-whitespace activate)
  (noflet ((delete-horizontal-space (&rest args) t))
    ad-do-it))

(defun unfill-region (beg end)
  "Remove all line breaks in a region but leave paragraphs,
  indented text (quotes, code) and lists intact."
  (interactive "r")

  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" end t)
      (replace-match "\\1 \\2"))))

(defun next-newline-and-indent ()
  "Insert new line *after* the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; diff- mode (better colors)
(require 'diff-mode-)

;; a slightly saner diff command
(require 'ediff)
(setq ediff-diff-options "-w")

;; tramp (remote files)
(load-after 'tramp
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (emacs-d "cache/tramp"))
  ;; cookies
  (setq url-cookie-file (emacs-d "cache/url/cookies")))

(defun sudo-open-file (&optional arg)
  "Find a file and open it as root."
  (interactive "p")
  (if arg
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun sudo-open-current-file ()
  "Edit the current file as root."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file
     (concat "/sudo::" (buffer-file-name (current-buffer))))
    (goto-char pos)))

(defun delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (y-or-n-p (concat "Delete file: " current-file))
      (kill-buffer (current-buffer))
      (delete-file current-file)
      (message (concat "Deleted file: " current-file)))))

(defun reload-current-file ()
  "Close the current buffer and re-open it. Typically used to quickly clear buffer variables and overlays."
  (interactive)
  (let ((current-file (buffer-file-name)))
    ;; makes only sense if there's no file associated with the buffer
    (when current-file
      (when (kill-buffer (current-buffer))
        (find-file current-file)))))

(defun trim-whitespace (&optional start end)
  "Slightly less aggressive version of 'delete-trailing-whitespace'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (copy-marker (or end (point-max))))
            (start (or start (point-min))))

        ;; delete trailing spaces
        (goto-char start)
        (while (re-search-forward "[ ]+$" end-marker t)
          (skip-chars-backward " " (line-beginning-position))
          (delete-region (point) (match-end 0)))

        ;; delete trailing spaces
        (goto-char start)
        (while (re-search-forward "^[\t]+$" end-marker t)
          (skip-chars-backward "\t" (line-beginning-position))
          (delete-region (point) (match-end 0)))

        ;; Delete trailing empty lines.
        (goto-char end-marker)
        (when (and (not end)
                   delete-trailing-lines
                   ;; Really the end of buffer.
                   (= (point-max) (1+ (buffer-size)))
                   (<= (skip-chars-backward "\n") -2))
          (delete-region (1+ (point)) end-marker))
        (set-marker end-marker nil))))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

;; remove trailing whitespace on save, unless it's a big buffer
;; TODO make this fast so we can always run it
(defun maybe-trim-whitespace ()
  (when (not (> (buffer-size) 1000000))
    (trim-whitespace)))

(add-hook 'before-save-hook 'maybe-trim-whitespace)

;; work with numbers at point
(require 'number)
(defun number/increment ()
  (interactive)
  (number/add (number-read "1")))
(defun number/decrement ()
  (interactive)
  (number/sub (number-read "1")))

;; smart parentheses
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)
(setq sp-show-pair-delay 0)

;; don't do any insertion/deletion magic
(setq       	sp-autoescape-string-quote             	nil)
(setq       	sp-autoinsert-pair                     	nil)
(setq       	sp-cancel-autoskip-on-backward-movement	nil)
(setq       	sp-autodelete-pair                     	nil)
(set-default	'sp-autoskip-opening-pair              	nil)
(set-default	'sp-autoskip-closing-pair              	nil)

;; deal with hungry-delete
(defadvice hungry-delete-backward (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))

;; sexp manipulation
(setq sp-hybrid-kill-excessive-whitespace t)
(setq sp-navigate-close-if-unbalanced t)

(defun sp-kill-to-end-of-sexp ()
  "Kill everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-end-of-sexp)
    (kill-region (mark) (point))))
(defun sp-kill-to-beginning-of-sexp ()
  "Kill everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-beginning-of-sexp)
    (kill-region (mark) (point))))
(defun sp-copy-to-end-of-sexp ()
  "Copy everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-end-of-sexp)
    (kill-ring-save (mark) (point))))
(defun sp-copy-to-beginning-of-sexp ()
  "copy everything in the sexp without unbalancing it."
  (interactive)
  (save-excursion
    (set-mark (point))
    (sp-beginning-of-sexp)
    (kill-ring-save (mark) (point))))

;; markdown-mode
(sp-with-modes '(markdown-mode)
  (sp-local-pair "*"	"*"	:actions	'(wrap)	)
  (sp-local-pair "_"	"_"	:actions	'(wrap)	)
  )

;; notes-mode
(sp-with-modes '(notes-mode)
  (sp-local-pair "*"	"*"	:actions	'(wrap)       	)
  (sp-local-pair "/"	"/"	:actions	'(wrap)       	)
  (sp-local-pair "<"	">"	:actions	'(wrap)       	)
  (sp-local-pair "["	"]"	:actions	'(wrap insert)	)
  (sp-local-pair "'"	"'"	:actions	'(wrap)       	)
  (sp-local-pair "`"	"`"	:actions	'(wrap)       	)
  (sp-local-pair "«"	"»"	:actions	'(wrap insert)	)
  )

;; overwrite |pipe| handling in ruby
(sp-with-modes '(enh-ruby-mode ruby-mode)
  (sp-local-pair "|"	"|"	:pre-handlers	nil)	)

;; normalize unicode in buffer
(defun normalize-unicode-in-buffer ()
  "Normalize Unicode in buffer to NFKC form."
  (interactive)
  (save-excursion
    (ucs-normalize-NFC-region (point-min) (point-max))))

;; normalize buffer before saving
(add-hook 'before-save-hook 'normalize-unicode-in-buffer)

;; transparently open compressed files
(auto-compression-mode t)

;; automatically create directories if necessary
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(defun toggle-title-case ()
  "Toggle case of the word / region between lower case and title case."
  (interactive)
  (let (region (word-or-region))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char (first region))
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "title"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (apply 'upcase-initials-region region)
      (put this-command 'state "title"))
     ((string= "title" (get this-command 'state))
      (apply 'downcase-region region)
      (put this-command 'state "lower")))
    ))

(defun toggle-upcase ()
  "Toggle case of the word / region between lower case and upper case."
  (interactive)
  (let (region (word-or-region))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char (first region))
        (cond
         ((looking-at "[[:lower:]]") (put this-command 'state "lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "upper"))
         (t (put this-command 'state "lower")))))

    (cond
     ((string= "lower" (get this-command 'state))
      (apply 'upcase-region region)
      (put this-command 'state "upper"))
     ((string= "upper" (get this-command 'state))
      (apply 'downcase-region region)
      (put this-command 'state "lower")))))

(defun title-case-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'upcase-initials-region (word-or-region)))

(defun upcase-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'upcase-region (word-or-region)))

(defun downcase-word-or-region ()
  "Transform word/region into title case."
  (interactive)
  (apply 'downcase-region (word-or-region)))

;; help extension
(require 'help-fns+)

;; alternative to C-q TAB for easier keybindings
(defun literal-tab ()
  (interactive)
  (insert "\t"))

(defun dos2unix ()
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

(defun scramble (beg end)
  "Scramble all lines in region."
  (interactive "r")

  (let ((beg	(min beg end))
        (end	(max beg end))
        lines
        done)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))

      (goto-char end)
      (when (bolp)
        (forward-char -1))
      (setq end (point))

      (setq lines (s-lines (buffer-substring-no-properties beg end)))

      (while (not done)
        (delete-region beg end)
        (insert (s-join "\n" (shuffle lines)))

        (setq done (/= (read-key "SPC to shuffle again") ?\s))))))

(defun shuffle (list)
  "fair permutation of a list"
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun add-lispish-hook (hook)
  (load-after 'lisp-mode           	
    (add-hook 'lisp-mode-hook      	hook)
    (add-hook 'emacs-lisp-mode-hook	hook))
  (load-after 'racket-mode         	
    (add-hook 'racket-mode-hook    	hook)))

;; better s-expression handling
(require 'adjust-parens)
(add-lispish-hook 'adjust-parens-mode)

(require 'lispy)
(add-lispish-hook 'lispy-mode)

(provide 'init-editing)
