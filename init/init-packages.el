;;; local packages

;; elpa package-repositories
(require 'package)
(setq package-user-dir (emacs-d "packages"))
(add-to-list 'package-archives '("tromey"   	. "https://tromey.com/elpa/")            	t)
(add-to-list 'package-archives '("marmalade"	. "https://marmalade-repo.org/packages/")	t)
(add-to-list 'package-archives '("melpa"    	. "https://melpa.milkbox.net/packages/") 	t)

(package-initialize)

(provide 'init-packages)
