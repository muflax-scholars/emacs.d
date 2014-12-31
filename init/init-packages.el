;;; local packages

;; elpa package-repositories
(setup "package"
  (setq package-user-dir (emacs-d "packages"))
  (add-to-list 'package-archives '("tromey"   	. "http://tromey.com/elpa/")                       	t)
  (add-to-list 'package-archives '("marmalade"	. "http://marmalade-repo.org/packages/")           	t)
  (add-to-list 'package-archives '("melpa"    	. "http://melpa.milkbox.net/packages/")            	t)
  (add-to-list 'package-archives '("SC"       	. "http://joseito.republika.pl/sunrise-commander/")	t)

  (defun package-disabled-packages ()
    (let (disabled-packages)
      (dolist (package package-alist)
        (let ((package-name (car package)))
          (unless (memq package-name package-activated-list)
            (add-to-list 'disabled-packages package-name))))

      disabled-packages))

  (defun package-delete-all-disabled ()
    (dolist (package (package-disabled-packages))
      (let ((version (elt (cdr (assoc package package-alist)) 0)))
        (package-delete
         (symbol-name package)
         (package-version-join version))))

    (dolist (package package-obsolete-alist)
      (dolist (version (cdr package))
        (package-delete
         (symbol-name (car package))
         (package-version-join (car version))))))

  (setq package-load-list '(all))

  (package-initialize))

(provide 'init-packages)
