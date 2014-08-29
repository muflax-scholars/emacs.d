;; load path
;; local stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/local/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
;; init has the startup scripts
(setq load-path (cons (expand-file-name "~/.emacs.d/init") load-path))

;; elpa package-repositories
(require 'package)
(setq package-user-dir "~/.emacs.d/packages")
(add-to-list 'package-archives '("tromey"    . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("SC"        . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)

(provide 'init-load-path)
