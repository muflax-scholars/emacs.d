;;; gitignore-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gitignore-mode) "gitignore-mode" "gitignore-mode.el"
;;;;;;  (21005 13275 847653 787000))
;;; Generated autoloads from gitignore-mode.el

(autoload 'gitignore-mode "gitignore-mode" "\
A major mode for editing .gitignore files.

\(fn)" t nil)

(dolist (pattern (list (rx "/.gitignore" string-end) (rx "/.git/info/exclude" string-end))) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;;***

;;;### (autoloads nil nil ("gitignore-mode-pkg.el") (21005 13275
;;;;;;  853563 673000))

;;;***

(provide 'gitignore-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitignore-mode-autoloads.el ends here
