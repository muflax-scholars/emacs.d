;;; newlisp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "newlisp-mode" "newlisp-mode.el" (21578 375
;;;;;;  483428 772000))
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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; newlisp-mode-autoloads.el ends here
