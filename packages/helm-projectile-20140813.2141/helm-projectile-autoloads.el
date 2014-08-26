;;; helm-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-projectile) "helm-projectile" "helm-projectile.el"
;;;;;;  (21499 64518 636513 791000))
;;; Generated autoloads from helm-projectile.el

(autoload 'helm-projectile "helm-projectile" "\
Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") 'helm-projectile) (define-key projectile-command-map (kbd "H") 'helm-projectile-switch-project)))

;;;***

;;;### (autoloads nil nil ("helm-projectile-pkg.el") (21499 64518
;;;;;;  658276 437000))

;;;***

(provide 'helm-projectile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-projectile-autoloads.el ends here
