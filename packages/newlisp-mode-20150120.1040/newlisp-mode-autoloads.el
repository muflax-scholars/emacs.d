;;; newlisp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (newlisp-mode newlisp-show-repl) "newlisp-mode"
;;;;;;  "newlisp-mode.el" (21703 47359 491634 0))
;;; Generated autoloads from newlisp-mode.el

(autoload 'newlisp-show-repl "newlisp-mode" "\
Display newlisp process buffer.

\(fn &optional NO-FOCUS)" t nil)

(defalias 'run-newlisp 'newlisp-show-repl)

(autoload 'newlisp-mode "newlisp-mode" "\
Major mode for editing newLISP code.

\(fn)" t nil)
 (add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))
 (add-to-list 'interpreter-mode-alist '("newlisp" . newlisp-mode))

;;;***

;;;### (autoloads nil nil ("newlisp-mode-pkg.el") (21703 47359 510465
;;;;;;  62000))

;;;***

(provide 'newlisp-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; newlisp-mode-autoloads.el ends here
