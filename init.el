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

;; external packages
(require 'init-packages)

;; helper functions (should be early)
(require 'init-helpers)

;; key bindings
(require 'init-keys)

;; themes and generic visual stuff
(require 'init-look)

;; editing features
(require 'init-align)
(require 'init-editing)
(require 'init-folding)
(require 'init-killing)
(require 'init-input)

;; navigation and searches
(require 'init-buffers)
(require 'init-navigation)
(require 'init-search)

;; auto-completion and templates
(require 'init-auto-completion)

;; major modes
(require 'init-major-modes)

;; mail
(require 'init-mail)

;; shell
(require 'init-shell)

;; random options
(require 'init-misc)

(message "emacs: ready!")
