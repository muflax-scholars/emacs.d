;; user data
(setq user-full-name    "muflax")
(setq user-mail-address "mail@muflax.com")

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load "~/.emacs.d/init-load-path.el")

;; init setup (for faster start-up)
(require 'setup)
(setup-initialize)

;; themes and generic visual stuff
(setup "init-look")

;; editing features
(setup "init-editing")

;; searches, navigation and a plethora of regex engines
(setup "init-search")

;; auto-completion and templates
(setup "init-auto-completion-nonsense")

;; major modes
(setup "init-major-modes")

;; random options
(setup "init-misc")

;; macros
(setup "init-macro")
