;; aligning things

;; old-school align; mostly just for completeness
(require 'align)
(defun align-region-or-current ()
  "Align current selected region or implied region if nothing is selected."
  (interactive)
  (if (and mark-active
           (/= (point) (mark)))
      (align (point) (mark))
    (align-current)))

;; repeat regex (teh fuck ain't that the default?!)
(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun align-whitespace (start end)
  "Align region by whitespace."
  (interactive "r")
  (align-regexp start end (concat "\\(\\s-*\\)" "\\s-") 1 0 t))

;; align should always indent with spaces
(defadvice align-areas (around fix-tab-indent activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; elastic tab stops
(require 'elastic-tabstops)

(setq elastic-tab-align-modes
      '(
        enh-ruby-mode
        notes-mode
        nix-mode
        emacs-lisp-mode
        lisp-mode
        racket-mode
        scheme-mode
        sh-mode
        js2-mode
        ))

(elastic-advice-command       	indent-for-tab-command)
(elastic-advice-command       	literal-tab)
(elastic-advice-command       	indent-according-to-mode)
(elastic-advice-command-region	indent-region)
(elastic-advice-command       	comment-dwim)
(elastic-advice-command-region	comment-region)
(elastic-advice-command-region	uncomment-region)

(add-hook 'notes-mode-hook 'elastic-turn-on-extended-columns)

(provide 'init-align)
