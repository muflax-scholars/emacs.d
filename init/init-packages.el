;;; local packages

;; elpa package-repositories
(require 'package)
(setq package-user-dir (emacs-d "packages"))
(add-to-list 'package-archives '("tromey"   	. "http://tromey.com/elpa/")                       	t)
(add-to-list 'package-archives '("marmalade"	. "http://marmalade-repo.org/packages/")           	t)
(add-to-list 'package-archives '("melpa"    	. "http://melpa.milkbox.net/packages/")            	t)

(package-initialize)

(provide 'init-packages)
