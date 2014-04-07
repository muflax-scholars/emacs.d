;; user data
(setq user-mail-address "mail@muflax.com")
(setq user-full-name "muflax")

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load "~/.emacs.d/load-path.el")

;; init setup (for faster start-up)
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

;; random options
(require 'misc)

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
