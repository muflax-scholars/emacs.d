;; major modes

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
(require 'guess-offset)
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)

;; use automatic file headers
;; #TODO recognize name automagically
;; #TODO add end-of-buffer
(require 'autoinsert)
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
(define-auto-insert "\\.go$"  "go")

;; auctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; markdown
(require 'markdown-mode)
(setq markdown-command "kramdown")
(add-to-list 'auto-mode-alist '("\\.pdc$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; notes-mode
(require 'notes-mode)
(add-to-list 'auto-mode-alist '("\\.txt$"      . notes-mode))
(add-to-list 'auto-mode-alist '("\\.notes$"    . notes-mode))
(add-to-list 'auto-mode-alist '("\\.script$"   . notes-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; org-mode (use private version)
;; #FIXME (tab) for org-cycle is disabled directly in the library; this should probably be some unset here.
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; loaded so that we can diminish it later
(require 'org-indent)
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

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; new python mode
(require 'python)
(setq python-indent-offset 2)

;; haskell mode
(require 'haskell-mode)
(require 'haskell-doc)
(require 'haskell-indentation)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
(define-key haskell-mode-map (kbd "C-c ?")   'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info)

;; ruby mode
;; enhanced ruby mode
(require 'enh-ruby-mode)
(setq enh-ruby-program "~/.rbenv/shims/ruby")
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
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
(define-key enh-ruby-mode-map (kbd "C-c C-n") 'enh-ruby-find-error)
(define-key enh-ruby-mode-map (kbd "C-c C-p") 'enh-ruby-beginning-of-defun)
;; misc stuff
(require 'yari)        ; ri documentation tool
(require 'ruby-block)  ; show what block an end belongs to
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;; better indenting
(setq ruby-block-highlight-toggle t)
(setq ruby-indent-level tab-width)
(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-deep-indent-paren nil)
;; Rake files are Ruby, too
(add-to-list 'auto-mode-alist '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
;; erb
(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.erb$"     . rhtml-mode))
;; auto-completion
(add-hook 'robe-mode-hook 'robe-ac-setup)
;; documentation
(define-key enh-ruby-mode-map (kbd "C-c ?") 'yari)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; shell stuff
(setq sh-basic-offset tab-width)

;; nxml stuff
(setq nxml-child-indent tab-width)

;; lua
(require 'lua-mode)
(setq lua-indent-level 2)

;; (s)css
(setq scss-compile-at-save nil)
(setq css-indent-level 2)

;; mark stuff like FIXME
(require 'fic-mode)
(add-hook 'prog-mode-hook     'fic-mode)
;; misbehaving modes
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook      'fic-mode)

;; dired
(require 'dired)
(require 'wdired)
(require 'dired-x)
(require 'dired-details)
(require 'dired-details+)
;; move files between split panes
(setq dired-dwim-target t)
;; reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))
(global-set-key (kbd "C-c C-j") 'dired-jump)
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

;; eldoc, ie function signatures in the minibuffer
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; go-lang
(require 'go-mode)
(require 'go-autocomplete)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook #'gofmt-before-save)
(setq gofmt-command "goimports")
(unbreak-stupid-map go-mode-map)
(define-key go-mode-map (kbd "M-t") 'godef-jump)
(define-key go-mode-map (kbd "M-T") 'godef-jump-other-window)

(provide 'major-modes)
