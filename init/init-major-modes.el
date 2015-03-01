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
(add-to-list 'interpreter-mode-alist '("ruby18" 	. enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby19" 	. enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby20" 	. enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby21" 	. enh-ruby-mode))

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

;; auctex
(load-lazy '(latex-mode LaTeX-mode tex-mode TeX-mode) "latex")
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; markdown
(load-lazy '(markdown-mode) "markdown-mode"
  (setq markdown-command "kramdown"))
(add-to-list 'auto-mode-alist '("\\.pdc$"     	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$"     	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"      	. markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$"	. markdown-mode))

;; notes-mode
(require 'notes-mode)
(add-hook 'notes-mode-hook 'leerzeichen-mode)
(add-to-list 'auto-mode-alist '("\\.txt$"   	. notes-mode))
(add-to-list 'auto-mode-alist '("\\.notes$" 	. notes-mode))
(add-to-list 'auto-mode-alist '("\\.script$"	. notes-mode))

;; yaml
(load-lazy '(yaml-mode) "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml$"	. yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" 	. yaml-mode))

;; org-mode
(load-lazy '(org-mode) "org"
  ;; proper indentation / folding
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)
  (setq org-blank-before-new-entry '((heading        	. nil)
                                     (plain-list-item	. auto)))
  (setq org-M-RET-may-split-line nil)

  ;; unset annoying keys
  (define-key org-mouse-map     	[(tab)]        	nil)
  (define-key org-goto-map      	[(tab)]        	nil)
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

  ;; code block
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp	. t)
     (sh        	. t)
     (ruby      	. t)
     (python    	. t)
     (haskell   	. t)))

  (add-to-list 'org-src-lang-modes '("ruby"	. enh-ruby))
  (add-to-list 'org-src-lang-modes '("r"   	. enh-ruby))
  (add-to-list 'org-src-lang-modes '("h"   	. haskell))
  (add-to-list 'org-src-lang-modes '("s"   	. sh))
  (add-to-list 'org-src-lang-modes '("p"   	. python))

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
  (setq python-indent-offset 2)
  (add-hook 'python-mode-hook (lambda () (setq tab-width 2))))

;; haskell mode
(load-lazy '(haskell-mode) "haskell-mode"
  (require 'haskell-doc)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

  (require 'haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (require 'inf-haskell)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; ruby mode
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(load-lazy '(ruby-mode enh-ruby-mode rhtml-mode) "enh-ruby-mode"
  (setq enh-ruby-program "~/.rbenv/shims/ruby")

  ;; flycheck covers errors anyway
  (setq enh-ruby-check-syntax nil)

  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent nil)
  (setq enh-ruby-deep-indent-paren nil)

  (add-hook 'enh-ruby-mode-hook 'leerzeichen-mode))

;;ruby files
(add-to-list 'interpreter-mode-alist	'("ruby"       	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.rake$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Rakefile$"  	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Gemfile$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("Capfile$"   	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.builder$"	. enh-ruby-mode))
(add-to-list 'auto-mode-alist       	'("\\.gemspec$"	. enh-ruby-mode))

(load-after 'enh-ruby-mode
  ;; ri documentation tool
  (require 'yari)

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
  (add-hook 'sh-mode-hook 'leerzeichen-mode))

;; nxml stuff
(load-lazy '(nxml-mode) "nxml-mode"
  (setq nxml-child-indent tab-width))

;; lua
(load-lazy '(lua-mode) "lua-mode"
  (setq lua-indent-level 2))

;; (s)css
(load-lazy '(scss-mode) "scss-mode"
  (setq scss-compile-at-save nil))
(load-lazy '(css-mode) "css-mode"
  (setq css-indent-level 2))

;; mark stuff like FIXME
(load-lazy '(fic-mode) "fic-mode")
(add-hook 'prog-mode-hook    	'fic-mode)
(add-hook 'enh-ruby-mode-hook	'fic-mode)
(add-hook 'js2-mode-hook     	'fic-mode)

;; dired
(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'dired-details)
(require 'dired-details+)
(require 'dired-open)

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

;; open by extension
(setq dired-open-extensions '(
                              ("pdf" 	. "zathura")
                              ("djvu"	. "zathura")
                              ))

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

  (add-hook 'emacs-lisp-mode-hook 'leerzeichen-mode))

;; ag search
(load-lazy '(ag) "ag"
  (setq ag-highlight-search t))

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
(load-lazy '(magit-status) "magit"
  (set-default 'magit-stage-all-confirm  	nil)
  (set-default 'magit-unstage-all-confirm	nil)

  (setq magit-log-cutoff-length 1000)

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

(load-lazy '(conf-mode) "conf-mode")

(load-lazy '(paradox-list-packages) "paradox"
  (setq paradox-github-token t))

(load-lazy '(nix-mode) "nix-mode"
  (add-hook 'nix-mode-hook 'leerzeichen-mode))
(add-to-list 'auto-mode-alist '("\\.nix" . nix-mode))

;; common lisp
(load-after 'lisp-mode
  (require 'slime-autoloads)
  (load-lazy '(slime) "slime"
    (setq slime-lisp-implementations
          '((ccl 	("ccl"))
            (sbcl	("sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)))

    (setq slime-default-lisp   	'sbcl)
    (setq inferior-lisp-program	"sbcl --noinform --no-linedit")

    (setq slime-contribs '(slime-fancy))
    (setq slime-enable-evaluate-in-emacs t)
    (setq slime-autodoc-use-multiline-p t)
    (setq slime-auto-start 'always)
    (setq slime-repl-history-file (emacs-d "cache/slime_history"))

    (add-hook 'lisp-mode-hook (lambda ()
                                (unless (slime-connected-p)
                                  (save-excursion (slime)))))
    )
  (add-hook 'lisp-mode-hook 'leerzeichen-mode))

(add-to-list 'auto-mode-alist '("\\.sbclrc$"	. lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$"    	. lisp-mode))

;; racket
(load-lazy '(racket-mode) "racket-mode"
  (require 'geiser-mode)
  (setq geiser-active-implementations '(racket))
  (setq geiser-default-implementation 'racket)
  (setq geiser-autodoc-delay 0.1)
  (setq geiser-repl-history-filename (emacs-d "cache/geiser_history"))
  (setq geiser-repl-company-p nil)
  (setq geiser-mode-company-p nil)
  (setq geiser-mode-start-repl-p t)

  ;; don't replace a lambda - if anything, use pretty-symbol-mode later
  (setq racket-mode-pretty-lambda nil)

  (add-hook 'racket-mode-hook 'geiser-mode--maybe-activate)
  (add-hook 'racket-mode-hook 'leerzeichen-mode))

(add-to-list 'auto-mode-alist '("\\.sc$" 	. scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scm$"	. scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss$" 	. scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$"	. racket-mode))

;; repl
(load-lazy '(ielm) "ielm"
  (setq ielm-prompt "> ")
  (add-hook 'ielm-mode-hook	'turn-on-eldoc-mode))

;; arc
(load-lazy '(arc-mode) "arc")
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))

;; shen
(load-lazy '(shen-mode) "shen-mode")
(add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))

(provide 'init-major-modes)
