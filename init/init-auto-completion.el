;; auto-completion nonsense

;; snippets
(require 'yasnippet)
(setq yas-snippet-dirs
      `(
        ,(emacs-d "snippets")
        ,(emacs-d "extra-snippets/german")
        ))

;; inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(defun yas-choose-from-file (filename)
  "Loads choices for 'yas-choose-value' from file, treating each line as an option."
  (if (file-exists-p filename)
      (yas-choose-value (read-lines filename))
    '("")))

(defun yas-choose-from-command (command)
  "Loads choices for 'yas-choose-value' from shell command, treating each line as an option."

  (yas-choose-value (split-string (shell-command-to-string command) "\n" t)))

(defun yas-insert-by-name (name)
  (noflet ((dummy-prompt
            (prompt choices &optional display-fn)
            (declare (ignore prompt))
            (or (cl-find name choices :key display-fn :test #'string=)
                (throw 'notfound nil))))
    (let ((yas-prompt-functions '(dummy-prompt)))
      (catch 'notfound
        (yas-insert-snippet t)))))

;; options
(setq yas-indent-line 'fixed)
(setq yas-verbosity 1)
(setq yas-wrap-around-region t)
(setq yas-triggers-in-field t)

;; no dropdowns
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

(yas-global-mode 1)

;; auto completion
(require 'company)
(setq company-idle-delay nil) ;; no idle completion because it's too slow
(setq company-tooltip-align-annotations t)
(global-company-mode)

(require 'company-quickhelp)
(company-quickhelp-mode 1)

;; fancy go autocompletion
(load-after 'go-mode
  (require 'company-go))

;; abbrev-mode
(load-after 'abbrev
  (setq abbrev-file-name (emacs-d "abbrev_defs"))
  (setq save-abbrevs 'silently)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

;; common-lisp
(load-after 'sly
  (require 'sly-company)
  (add-hook 'sly-mode-hook 	'sly-company-mode)
  (add-hook 'sly-mrepl-hook	'sly-company-mode))

;; rust
(load-after 'racer
  (add-hook 'racer-mode-hook #'company-mode))

;; ocaml
(load-after 'merlin
  (add-to-list 'company-backends 'merlin-company-backend))

;; elm
(load-after 'elm-mode
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm))

;; use automatic file headers
;; #TODO recognize name automagically
(require 'autoinsert)
(defun auto-insert-from-yas (ext mode)
  (define-auto-insert
    (format "\\.%s$" ext)
    (lambda ()
      (yas-insert-by-name "shebang")
      (end-of-buffer))))

(defun current-year ()
  (format-time-string "%Y"))

(auto-insert-mode)
(setq auto-insert-directory (emacs-d "templates/"))
(setq auto-insert-query nil)

;; don't use the default values
(setq auto-insert-alist '())

(auto-insert-from-yas "sh"  	'sh-script-mode)
(auto-insert-from-yas "py"  	'python-mode)
(auto-insert-from-yas "hs"  	'haskell-mode)
(auto-insert-from-yas "pl"  	'perl-mode)
(auto-insert-from-yas "rb"  	'ruby-mode)
(auto-insert-from-yas "c"   	'c-mode)
(auto-insert-from-yas "h"   	'c-mode)
(auto-insert-from-yas "cpp" 	'c-mode)
(auto-insert-from-yas "go"  	'go-mode)
(auto-insert-from-yas "rkt" 	'racket-mode)
(auto-insert-from-yas "rs"  	'rust-mode)
(auto-insert-from-yas "ml"  	'tuareg-mode)
(auto-insert-from-yas "lisp"	'lisp-mode)

(provide 'init-auto-completion)
