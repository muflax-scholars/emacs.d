;; user data
(setq user-full-name    "muflax")
(setq user-mail-address "mail@muflax.com")

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load "~/.emacs.d/load-path.el")

;; init setup (for faster start-up)
(require 'setup)
(setup-initialize)

;; themes and generic visual stuff
(setup "look")

;; editing features
(setup "editing")

;; auto-completion and templates
(setup "auto-completion-nonsense")

;; major modes
(setup "major-modes")

;; random options
(setup "misc")

(setq initial-major-mode 'notes-mode)
