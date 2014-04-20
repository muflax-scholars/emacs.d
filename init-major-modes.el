;; major modes

;; default modes
(setup-after "notes-mode"
  (setq-default major-mode 'notes-mode)
  (setq initial-major-mode 'notes-mode))

;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; fuck you, you key-stealing whore
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

;; associate non-standardish interpreters with modes
(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))
(add-to-list 'interpreter-mode-alist '("ruby18"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby20"  . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby21"  . enh-ruby-mode))

;; c style (1TBS, but guess offsets for other files)
(setq c-default-style "k&r" c-basic-offset tab-width)
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)
(setup "guess-offset")

;; eldoc for function signatures
(setup-lazy '(c-turn-on-eldoc-mode) "c-eldoc"
  (setq c-eldoc-buffer-regenerate-time 15))
(setup-after "cc-mode"
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

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
(setup-lazy '(yaml-mode) "yaml-mode"
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode)))

;; org-mode (use private version)
;; #FIXME (tab) for org-cycle is disabled directly in the library; this should probably be some unset here.
(setq load-path (cons "~/.emacs.d/local/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setup-lazy '(org-mode) "org"

  ;; proper indentation / folding
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)
  (setq org-blank-before-new-entry '(
                                     (heading . nil)
                                     (plain-list-item . auto)))
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
  ;; keybindings
  (org-defkey org-mode-map "\C-c\C-t" (lambda () (interactive) (org-todo "TODO")))
  (org-defkey org-mode-map "\C-c\C-w" (lambda () (interactive) (org-todo "WAITING")))
  (org-defkey org-mode-map "\C-c\C-d" (lambda () (interactive) (org-todo "DONE")))
  ;; shortcut for C-u C-c C-l
  (defun org-insert-file-link () (interactive) (org-insert-link '(4)))
  ;; some templates
  (setcdr (assoc "c" org-structure-template-alist)
          '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
  (add-to-list 'org-structure-template-alist
               '("r"
                 "#+BEGIN_SRC ruby\n?\n#+END_SRC"
                 "<src lang=\"ruby\">\n\n</src>"))
  )

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
  (unbreak-stupid-map python-mode-map))

;; haskell mode
(setup-lazy '(haskell-mode) "haskell-mode")
(setup-after "haskell-mode"
  (setup "haskell-doc"
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode))
 (setup "haskell-indentation"
   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
 (setup "inf-haskell"
   (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
   (define-key haskell-mode-map (kbd "C-c ?")   'haskell-process-do-type)
   (define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info)))

;; ruby mode
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(setup-lazy '(ruby-mode enh-ruby-mode) "enh-ruby-mode"
  (setq enh-ruby-program "~/.rbenv/shims/ruby")

  ;; better colors for warnings
  (defface erm-syn-warnline
    '((t (:underline "orange")))
    "Face used for marking warning lines."
    :group 'enh-ruby)
  (defface erm-syn-errline
    '((t (:background "pink")))
    "Face used for marking error lines."
    :group 'enh-ruby)
  (unbreak-stupid-map enh-ruby-mode-map)

  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren nil)
  )

;;ruby files
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))

(setup-after "enh-ruby-mode"
;; misc stuff
  (setup "yari"       ; ri documentation tool
    (define-key enh-ruby-mode-map (kbd "C-c ?") 'yari))
  (setup "ruby-block" ; show what block an end belongs to
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  (setup "rhtml-mode" ; erb
    (add-to-list 'auto-mode-alist '("\\.erb$"     . rhtml-mode))))

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
    (dired-next-line 1)
    (dired-find-file))

  (defun dired-prev ()
    (interactive)
    (dired-jump)
    (dired-next-line -1)
    (dired-find-file))

  ;; move files between split panes
  (setq dired-dwim-target t)

  ;; less confirmations
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  )

(global-set-key (kbd "C-c C-j") 'dired-jump)
(global-set-key (kbd "C-c C-n") 'dired-next)
(global-set-key (kbd "C-c C-p") 'dired-prev)

(setup-after "dired"
  (setup "wdired")
  (setup "dired-x")
  (setup "dired-details")
  (setup "dired-details+")
  (setup "dired-open")

  ;; don't ask for confirmation
  (setq dired-no-confirm t)
  ;; reload dired after making changes
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))
  (define-key dired-mode-map (kbd "C-c C-c")  'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "<insert>") 'dired-mark)
  ;; C-a goes to filename
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  ;; M-up goes to first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
  ;; M-down goes to last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

  (defun dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (define-key dired-mode-map (kbd ".") 'dired-dotfiles-toggle)

  ;; open by extension
  (setq dired-open-extensions '(
                               ("pdf"  . "zathura")
                               ("djvu" . "zathura")
                               ))
  )

;; eldoc, ie function signatures in the minibuffer
(setup-lazy '(turn-on-eldoc-mode) "eldoc"
  (setq eldoc-idle-delay 0.1))
(setup-after "lisp-mode"
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; go-lang
(setup-lazy '(go-mode) "go-mode"
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (unbreak-stupid-map go-mode-map)
  (define-key go-mode-map (kbd "M-t") 'godef-jump)
  (define-key go-mode-map (kbd "M-T") 'godef-jump-other-window))

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

;; ag search
(setup-lazy '(ag) "ag"
  (setq ag-highlight-search t))

;; Flycheck for code linting
(setup "flycheck"
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (unbreak-stupid-map flycheck-mode-map)
  (setq flycheck-mode-line-lighter " !")
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; disable version control in emacs because it just bloats the mode-line
(setup "vc"
  (setq vc-handled-backends ()))

;; fancy git interactions
(setup-lazy '(magit-status) "magit"
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)

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

  ;; needed because of fullscreen override
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(global-set-key (kbd "C-x g") 'magit-status)

(setup-after "magit"
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

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(provide 'init-major-modes)
