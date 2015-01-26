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

;; auto-yasnippet
(load-after 'yasnippet
  (setup-lazy '(aya-create aya-expand) "auto-yasnippet"))

;; auto completion
(require 'auto-complete-config)
(add-to-list 'ac-modes   	'enh-ruby-mode)
(add-to-list 'ac-modes   	'go-mode)
;; (add-to-list 'ac-modes	'notes-mode)

(add-to-list 'ac-dictionary-directories (emacs-d "ac-dict"))
(setq ac-comphist-file (emacs-d "cache/ac-comphist.dat"))
(setq ac-auto-show-menu nil)
(setq ac-ignore-case nil)
(setq ac-quick-help-delay 0.8)
(ac-config-default)

;; fancy go autocompletion
(load-after 'go-mode
  (require 'go-autocomplete))

;; abbrev-mode
(load-after 'abbrev
  (setq abbrev-file-name (emacs-d "abbrev_defs"))
  (setq save-abbrevs 'silently)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

;; racket repl
(load-after 'racket-mode
  (require 'ac-geiser)
  (add-hook 'geiser-mode-hook     	'ac-geiser-setup)
  (add-hook 'geiser-repl-mode-hook	'ac-geiser-setup)

  (add-to-list 'ac-modes 'racket-mode)
  (add-to-list 'ac-modes 'geiser-repl-mode))

;; common lisp repl
(load-after 'lisp-mode
  (require 'ac-slime)
  (add-hook 'slime-mode-hook     	'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook	'set-up-slime-ac)

  (add-to-list 'ac-modes 'common-lisp-mode)
  (add-to-list 'ac-modes 'slime-repl-mode))

;; emacs-lisp
(load-after 'ielm
  (add-hook 'ielm-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode))

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

(auto-insert-from-yas "sh" 	'sh-script-mode)
(auto-insert-from-yas "py" 	'python-mode)
(auto-insert-from-yas "hs" 	'haskell-mode)
(auto-insert-from-yas "pl" 	'perl-mode)
(auto-insert-from-yas "rb" 	'ruby-mode)
(auto-insert-from-yas "c"  	'c-mode)
(auto-insert-from-yas "h"  	'c-mode)
(auto-insert-from-yas "cpp"	'c-mode)
(auto-insert-from-yas "go" 	'go-mode)
(auto-insert-from-yas "rkt"	'racket-mode)

(provide 'init-auto-completion)
