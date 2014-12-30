;; user data
(setq user-full-name   	"muflax")
(setq user-mail-address	"mail@muflax.com")

;; load path
(defun emacs-d (path)
  (let ((user-dir
         (cond ((boundp 'user-init-dir)       	user-init-dir)
               ((boundp 'user-emacs-directory)	user-emacs-directory)
               (t                             	"~/.emacs.d/"))))
    (concat user-dir path)))
(load (emacs-d "init/init-load-path.el"))

;; init setup (for faster start-up)
(require 'setup)
(setup-initialize)

;; external packages
(setup "init-packages")

;; helper functions (should be early)
(setup "init-helpers")

;; key bindings
(setup "init-keys")

;; themes and generic visual stuff
(setup "init-look")

;; editing features
(setup "init-editing")

;; searches, navigation and a plethora of regex engines
(setup "init-search")

;; auto-completion and templates
(setup "init-auto-completion")

;; major modes
(setup "init-major-modes")

;; mail
(setup "init-mail")

;; shell
(setup "init-shell")

;; random options
(setup "init-misc")
