;; load path; site-lisp stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/local/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; user data
(setq user-mail-address "mail@muflax.com")
(setq user-full-name "muflax")

;; elpa package-repositories
(require 'package)
(setq package-user-dir "~/.emacs.d/packages")
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; init setup
(require 'cl)
(require 'setup)
(setup-initialize)

;; themes and generic visual stuff
(require 'look)

;; editing features
(require 'editing)

;; auto-completion and templates
(require 'auto-completion-nonsense)

;; major modes
(require 'major-modes)

;; other options

;; calendar
(setq calendar-week-start-day 1)  ; monday
(setq european-calendar-style 't) ; sanity

;; custom variables because fuck emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; make sure we always know what's happening when eval-ing things
(setq eval-expression-print-level nil)

;; clean up modeline and hide standard minor modes
;; should be last so all modes are already loaded
(require 'diminish)
(diminish 'auto-complete-mode)
(diminish 'auto-fill-function "AF")
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'fic-mode)
(diminish 'global-visual-line-mode)
(diminish 'global-whitespace-mode "WS")
(diminish 'haskell-doc-mode)
(diminish 'haskell-indentation-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'hs-minor-mode)
(diminish 'ruby-block-mode)
(diminish 'smartparens-mode)
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'volatile-highlights-mode)
(diminish 'whitespace-mode " »")
(diminish 'whole-line-or-region-mode)
(diminish 'yas-minor-mode)
