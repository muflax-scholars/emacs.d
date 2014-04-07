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
