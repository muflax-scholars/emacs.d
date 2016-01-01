;; major modes

;; default modes
(load-after 'notes-mode
  (setq-default major-mode 'notes-mode)
  (setq initial-major-mode 'notes-mode))

;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2"	. python-mode))
(add-to-list 'interpreter-mode-alist '("python3"	. python-mode))

;; c style (1TBS, but guess offsets for other files)
(setq c-default-style "k&r" c-basic-offset tab-width)
(require 'guess-offset)

;; eldoc for function signatures
(load-lazy '(c-turn-on-eldoc-mode) "c-eldoc"
  (setq c-eldoc-buffer-regenerate-time 15))
(load-after 'cc-mode
  (add-hook 'c++-mode-hook	'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook  	'c-turn-on-eldoc-mode))

;; show what function we're in
(require 'which-func)
(which-function-mode 1)

;; markdown
(load-lazy '(markdown-mode) "markdown-mode"
  (setq markdown-command "kramdown"))
(add-to-list 'auto-mode-alist '("\\.pdc$"     	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$"     	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"      	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$"	. markdown-mode))

;; notes-mode
(require 'notes-mode)
(add-to-list 'auto-mode-alist '("\\.txt$"   	. notes-mode))

;; yaml
(load-lazy '(yaml-mode) "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml$"	. yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" 	. yaml-mode))

;; org-mode
(load-lazy '(org-mode) "org"
  ;; proper indentation / folding
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level tab-width)
  (setq org-startup-folded 'content)
  (setq org-blank-before-new-entry '((heading        	. nil)
                                     (plain-list-item	. auto)))
  (setq org-M-RET-may-split-line nil)

  ;; unset annoying keys
  (define-key org-mouse-map     	[(tab)]        	nil)
  (define-key orgstruct-mode-map	[(tab)]        	nil)
  (define-key orgstruct-mode-map	(kbd "C-i")    	nil)
  (define-key org-mode-map      	[(tab)]        	nil)
  (define-key org-mode-map      	[(control tab)]	nil)
  (define-key org-mode-map      	(kbd "M-t")    	nil)
  (define-key org-mode-map      	[S-iso-lefttab]	nil)
  (define-key org-mode-map      	[(shift tab)]  	nil)
  (define-key org-mode-map      	[backtab]      	nil)
  (define-key org-mode-map      	[?\e (tab)]    	nil)

  ;; tag column
  (setq org-tags-column -70)

  ;; dependencies
  (setq org-enforce-todo-dependencies t)

  ;; todo states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))

  ;; priorities
  (setq org-default-priority 67) ;C

  (defmacro org-todo-fun (name)
    `(defun ,(intern (format "org-todo-%s" (downcase name))) ()
       (interactive)
       (org-todo name)))

  (org-todo-fun "TODO")
  (org-todo-fun "WAITING")
  (org-todo-fun "DONE")
  )

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; loaded so that we can diminish it later
(load-after 'org-mode
  (require 'org-indent))

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert some special buffers
(add-hook 'dired-mode-hook  	'turn-on-auto-revert-mode)
(add-hook 'neotree-mode-hook	'turn-on-auto-revert-mode)
(setq auto-revert-verbose nil)

;; FIXME doesn't work with symlinked directories right now
(setq auto-revert-use-notify nil)

;; imenu works badly across reverts, so just flush
(add-hook 'after-revert-hook 'imenu-flush-cache)

;; new python mode
(load-lazy '(python-mode) "python"
  (setq python-indent-offset tab-width)
  (add-hook 'python-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'python-mode-hook (lambda () (electric-indent-local-mode -1)))
  )

;; ruby mode
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(load-lazy '(ruby-mode enh-ruby-mode rhtml-mode) "enh-ruby-mode"
  (setq enh-ruby-program "ruby")

  ;; flycheck covers errors anyway
  (setq enh-ruby-check-syntax nil)

  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent nil)
  (setq enh-ruby-deep-indent-paren nil)
  )

;;ruby files
(add-to-list 'interpreter-mode-alist	'("ruby"       	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.rake$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.rb$"     	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Rakefile$"  	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Gemfile$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Capfile$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.builder$"	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.gemspec$"	. enh-ruby-mode))

(load-after 'enh-ruby-mode
  ;; show what block an end belongs to
  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t)

  ;; erb
  (require 'rhtml-mode))

(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; javascript
(load-lazy '(js2-mode) "js2-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; shell stuff
(load-lazy '(sh-mode) "sh-script"
  (setq sh-basic-offset tab-width)
  (setq sh-indentation tab-width))

;; nxml stuff
(load-lazy '(nxml-mode) "nxml-mode"
  (setq nxml-child-indent tab-width))

;; lua
(load-lazy '(lua-mode) "lua-mode"
  (setq lua-indent-level tab-width))

;; (s)css
(load-lazy '(scss-mode) "scss-mode"
  (setq scss-compile-at-save nil))
(load-lazy '(css-mode) "css-mode"

;; mark stuff like FIXME
(load-lazy '(fic-mode) "fic-mode")
(add-hook 'prog-mode-hook    	'fic-mode)
(add-hook 'enh-ruby-mode-hook	'fic-mode)
(add-hook 'js2-mode-hook     	'fic-mode)
  (setq css-indent-level tab-width))

;; dired
(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'dired-details)
(require 'dired-details+)

;; fast navigation through files in a directory
;; TODO this is super simplistic, but meh
(defun dired-next ()
  (interactive)
  (dired-jump)
  (revert-buffer)
  (dired-next-line 1)
  (when (eobp)
    (dired-back-to-top))
  (dired-find-file))

(defun dired-prev ()
  (interactive)
  (dired-jump)
  (revert-buffer)
  (dired-next-line -1)
  (when (looking-at "\\.\\.?$")
    (dired-jump-to-bottom))
  (dired-find-file))

;; reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

;; just delete files, sheesh
(defadvice dired-clean-up-after-deletion (around quiet-delete activate)
  (noflet ((y-or-n-p (&rest args) t))
    ad-do-it))

;; move files between split panes
(setq dired-dwim-target t)

;; don't ask for confirmation in most situations
(setq dired-no-confirm t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;; use omit-mode to hide dotfiles
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\(\\..*[^.]\\|\\.\\)$")
(setq dired-omit-verbose nil)

;; sort number naturally
(setq dired-listing-switches "--group-directories-first -v -al")

;; eldoc, ie function signatures in the minibuffer
(load-lazy '(turn-on-eldoc-mode) "eldoc"
  (setq eldoc-idle-delay 0.1))
(load-after 'lisp-mode
  (add-hook 'lisp-mode-hook      	'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook	'turn-on-eldoc-mode))

;; go-lang
(load-lazy '(go-mode) "go-mode"
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq gofmt-show-errors nil))

(load-after 'go-mode
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; emacs-lisp
(require 'bytecomp)
(defun byte-compile-update-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'byte-compile-update-current-buffer)

(load-after 'lisp-mode
  ;; highlight common libraries
  (require 'cl-lib-highlight)
  (cl-lib-highlight-initialize)

  (require 'dash)
  (dash-enable-font-lock)

  (require 'lisp-extra-font-lock)
  ;; make sure it's loaded late so whitespace etc are still fine
  (add-hook 'lisp-mode-hook 'lisp-extra-font-lock-mode t)
  )

;; Flycheck for code linting
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-indication-mode 'right-fringe)
(setq flycheck-display-errors-function nil)
(setq-default flycheck-disabled-checkers
              '(emacs-lisp-checkdoc
                ruby-rubocop))

;; disable version control in emacs because it just bloats the mode-line
(require 'vc)
(setq vc-handled-backends ())

;; fancy git interactions
;; disable recent warnings
(setq magit-last-seen-setup-instructions "1.4.0")
(load-lazy '(magit-status) "magit"
  (set-default 'magit-stage-all-confirm  	nil)
  (set-default 'magit-unstage-all-confirm	nil)

  (setq git-commit-summary-max-length 80)
  (setq magit-log-cutoff-length 1000)
  (setq magit-diff-auto-show '())
  (setq magit-push-always-verify nil)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-arguments)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-arguments "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-arguments (remove "-w" magit-diff-arguments))
    (magit-refresh))
  )

(load-lazy '(conf-mode) "conf-mode")

(load-lazy '(paradox-list-packages) "paradox"
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously nil))

;; repl
(load-lazy '(ielm) "ielm"
  (setq ielm-prompt "> ")
  (add-hook 'ielm-mode-hook	'turn-on-eldoc-mode))

;; rust
(load-lazy '(rust-mode) "rust-mode"
  (setq rust-indent-offset tab-width)
  (setq rust-indent-method-chain t)
  (setq rust-blink-matching-angle-brackets nil)

  (add-hook 'rust-mode-hook 'abbrev-mode)

  (load-after 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  )

(defun save-compile-run ()
  "Save the current buffer, ask for (and remember) compile command, and run it."
  (interactive)
  (save-buffer)

  ;; some simple defaults
  (let ((command (case major-mode
                   (c-mode     	"make")
                   (rust-mode  	"cargo build")
                   (racket-mode	"")
                   (notes-mode 	"")
                   (t          	""))))

    (setq-local compile-command
                (if (s-blank? compile-command)
                    command
                  compile-command)))

  ;; don't ask for a command unless we C-u for it
  (setq-local compilation-read-command nil)

  (unless (s-blank? compile-command)
    (call-interactively 'compile)))

(require 'compile)
(setq compilation-always-kill t)
(setq compilation-auto-jump-to-first-error nil)
(setq compile-command "")

;; color the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (search-forward "warning" nil t))))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; racket
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
(add-to-list 'interpreter-mode-alist '("racket" . racket-mode))

;; perl6
(load-lazy '(perl6-mode) "perl6-mode"
  (setq perl6-indent-offset tab-width))
(add-to-list 'auto-mode-alist '("\\.p[lm]?6$" 	. perl6-mode))
(add-to-list 'auto-mode-alist '("\\.nqp$"     	. perl6-mode))
(add-to-list 'interpreter-mode-alist '("perl6"	. perl6-mode))

(provide 'init-major-modes)
