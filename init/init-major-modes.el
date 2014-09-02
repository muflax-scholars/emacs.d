;; major modes

;; default modes
(setup-after "notes-mode"
  (setq-default major-mode 'notes-mode)
  (setq initial-major-mode 'notes-mode))

;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("ruby18"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby20"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby21"  . enh-ruby-mode))

;; c style (1TBS, but guess offsets for other files)
(setq c-default-style "k&r" c-basic-offset tab-width)
(setup "guess-offset")

;; eldoc for function signatures
(setup-lazy '(c-turn-on-eldoc-mode) "c-eldoc"
  (setq c-eldoc-buffer-regenerate-time 15))
(setup-after "cc-mode"
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

;; show what function we're in
(setup "which-func"
  (which-function-mode 1))

;; use automatic file headers
;; #TODO recognize name automagically
;; #TODO add end-of-buffer
(setup "autoinsert"
  (auto-insert-mode)
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (setq auto-insert-query nil)
  (define-auto-insert "\\.sh$"  "sh")
  (define-auto-insert "\\.py$"  "python")
  (define-auto-insert "\\.hs$"  "haskell")
  (define-auto-insert "\\.pl$"  "perl")
  (define-auto-insert "\\.rb$"  "ruby")
  (define-auto-insert "\\.c$"   "c")
  (define-auto-insert "\\.cpp$" "cpp")
  (define-auto-insert "\\.go$"  "go"))

;; auctex
(setup-lazy '(latex-mode LaTeX-mode tex-mode TeX-mode) "latex")
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; markdown
(setup-lazy '(markdown-mode) "markdown-mode"
  (setq markdown-command "kramdown"))
(add-to-list 'auto-mode-alist '("\\.pdc$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; notes-mode
(setup "notes-mode")
(add-to-list 'auto-mode-alist '("\\.txt$"    . notes-mode))
(add-to-list 'auto-mode-alist '("\\.notes$"  . notes-mode))
(add-to-list 'auto-mode-alist '("\\.script$" . notes-mode))

;; yaml
(setup-lazy '(yaml-mode) "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))

;; org-mode
(setup-lazy '(org-mode) "org"
  ;; proper indentation / folding
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto)))
  (setq org-M-RET-may-split-line nil)

  ;; unset annoying keys
  (define-key org-mouse-map      [(tab)]         nil)
  (define-key org-goto-map       [(tab)]         nil)
  (define-key orgstruct-mode-map [(tab)]         nil)
  (define-key orgstruct-mode-map (kbd "C-i")     nil)
  (define-key org-mode-map       [(tab)]         nil)
  (define-key org-mode-map       [(control tab)] nil)
  (define-key org-mode-map       (kbd "M-t")     nil)
  (define-key org-mode-map       [S-iso-lefttab] nil)
  (define-key org-mode-map       [(shift tab)]   nil)
  (define-key org-mode-map       [backtab]       nil)
  (define-key org-mode-map       [?\e (tab)]     nil)

  ;; tag column
  (setq org-tags-column -70)

  ;; dependencies
  (setq org-enforce-todo-dependencies t)

  ;; todo states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))

  ;; priorities
  (setq org-default-priority 67) ;C

  ;; code block
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh         . t)
     (ruby       . t)
     (python     . t)
     (haskell    . t)))
  (add-to-list 'org-src-lang-modes '("ruby" . enh-ruby))
  (add-to-list 'org-src-lang-modes '("r"    . enh-ruby))
  (add-to-list 'org-src-lang-modes '("h"    . haskell))
  (add-to-list 'org-src-lang-modes '("s"    . sh))
  (add-to-list 'org-src-lang-modes '("p"    . python))
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)

  ;; shortcut for C-u C-c C-l
  (defun org-insert-file-link ()
    (interactive)
    (org-insert-link '(4)))

  ;; some templates
  (setcdr (assoc "c" org-structure-template-alist)
          '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
  (add-to-list 'org-structure-template-alist
               '("r"
                 "#+BEGIN_SRC ruby\n?\n#+END_SRC"
                 "<src lang=\"ruby\">\n\n</src>"))

  (defun org-todo-todo ()
    (interactive)
    (org-todo "TODO"))

  (defun org-todo-waiting ()
    (interactive)
    (org-todo "WAITING"))

  (defun org-todo-done ()
    (interactive)
    (org-todo "DONE"))

  )
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; loaded so that we can diminish it later
(setup-after "org-mode"
  (setup "org-indent"))

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert dired
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
(setq auto-revert-verbose nil)

;; new python mode
(setup-lazy '(python-mode) "python"
  (setq python-indent-offset 2)
  (add-hook 'python-mode-hook (lambda () (setq tab-width 2))))

;; haskell mode
(setup-lazy '(haskell-mode) "haskell-mode")
(setup-after "haskell-mode"
  (setup "haskell-doc"
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode))
 (setup "haskell-indentation"
   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
 (setup "inf-haskell"
   (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)))

;; ruby mode
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(setup-lazy '(ruby-mode enh-ruby-mode rhtml-mode) "enh-ruby-mode"
  (setq enh-ruby-program "~/.rbenv/shims/ruby")

  ;; flycheck covers errors anyway
  (setq enh-ruby-check-syntax nil)

  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren nil))

;;ruby files
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))

(setup-after "enh-ruby-mode"
  ;; ri documentation tool
  (setup "yari")

  ;; show what block an end belongs to
  (setup "ruby-block"
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))

  ;; erb
  (setup "rhtml-mode"))
(add-to-list 'auto-mode-alist '("\\.erb$"     . rhtml-mode))

;; javascript
(setup-lazy '(js2-mode) "js2-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; shell stuff
(setup-lazy '(shell-mode) "sh-script"
  (setq sh-basic-offset tab-width))

;; nxml stuff
(setup-lazy '(nxml-mode) "nxml-mode"
  (setq nxml-child-indent tab-width))

;; lua
(setup-lazy '(lua-mode) "lua-mode"
  (setq lua-indent-level 2))

;; (s)css
(setup-lazy '(scss-mode) "scss-mode"
  (setq scss-compile-at-save nil))
(setup-lazy '(css-mode) "css-mode"
  (setq css-indent-level 2))

;; mark stuff like FIXME
(setup-lazy '(fic-mode) "fic-mode")
(add-hook 'prog-mode-hook     'fic-mode)
;; misbehaving modes
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook      'fic-mode)

;; dired
(setup-lazy '(dired-jump dired-next dired-prev) "dired"

  ;; fast navigation through files in a directory
  ;; TODO this is super simplistic, but meh
  (defun dired-next ()
    (interactive)
    (dired-jump)
    (revert-buffer)
    (dired-next-line 1)
    (dired-find-file))

  (defun dired-prev ()
    (interactive)
    (dired-jump)
    (revert-buffer)
    (dired-next-line -1)
    (dired-find-file))

  ;; move files between split panes
  (setq dired-dwim-target t)

  ;; don't ask for confirmation in most situations
  (setq dired-no-confirm t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  )

(setup-after "dired"
  (setup "wdired")
  (setup "dired-x")
  (setup "dired-details")
  (setup "dired-details+")
  (setup "dired-open")

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
    (beginning-of-buffer)
    (dired-next-line 4))

  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))

  ;; use omit-mode to hide dotfiles
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\..*[^.]$")
  (setq dired-omit-verbose nil)

  ;; open by extension
  (setq dired-open-extensions '(
                               ("pdf"  . "zathura")
                               ("djvu" . "zathura")
                               ))

  ;; sort number naturally
  (setq dired-listing-switches "--group-directories-first -v -al"))

;; eldoc, ie function signatures in the minibuffer
(setup-lazy '(turn-on-eldoc-mode) "eldoc"
  (setq eldoc-idle-delay 0.1))
(setup-after "lisp-mode"
  (add-hook 'lisp-mode-hook 'turn-on-eldoc-mode))

;; go-lang
(setup-lazy '(go-mode) "go-mode"
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq gofmt-show-errors nil))

(setup-after "go-mode"
  (setup "go-eldoc"
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; emacs-lisp
(setup "bytecomp"
  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'byte-compile-current-buffer))

(setup-after "emacs-lisp-mode"
  ;; highlight common libraries
  (setup "cl-lib-highlight" (cl-lib-highlight-initialize))
  (setup "dash"             (dash-enable-font-lock)))

;; ag search
(setup-lazy '(ag) "ag"
  (setq ag-highlight-search t))

;; Flycheck for code linting
(setup "flycheck"
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-display-errors-function nil)
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; disable version control in emacs because it just bloats the mode-line
(setup "vc"
  (setq vc-handled-backends ()))

;; fancy git interactions
(setup-lazy '(magit-status) "magit"
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-log-cutoff-length 10000)

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
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh))
  )

(setup-lazy '(conf-mode) "conf-mode")

(setup-lazy '(paradox-list-packages) "paradox"
  (setq paradox-github-token t))

(setup-lazy '(nix-mode) "nix-mode")
(add-to-list 'auto-mode-alist '("\\.nix" . nix-mode))

;; common lisp
(setup-lazy '(slime) "slime"
  (setq quicklisp-helper-path "~/local/quicklisp/slime-helper.el")
  (when (file-exists-p (expand-file-name quicklisp-helper-path))
    (load (expand-file-name quicklisp-helper-path)))

  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy))
  (setq slime-enable-evaluate-in-emacs t)
  (setq slime-autodoc-use-multiline-p t)
  (setq slime-auto-start 'always)
  )

(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$"     . lisp-mode))

;; racket
(setup "racket-mode"
  (setup "geiser-mode"
    (setq geiser-active-implementations '(racket))
    (setq geiser-default-implementation 'racket)
    (setq geiser-autodoc-delay 0.1)
    (setq geiser-repl-history-filename "~/.emacs.d/cache/geiser_history")
    (setq geiser-repl-company-p nil)
    (setq geiser-mode-company-p nil)
    (setq geiser-mode-start-repl-p t)

    (add-hook 'racket-mode-hook 'geiser-mode--maybe-activate))
  )
(add-to-list 'auto-mode-alist '("\\.scm$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))


(provide 'init-major-modes)
