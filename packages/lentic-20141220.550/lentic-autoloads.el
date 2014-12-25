;;; lentic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-lentic-start-mode lentic-start-mode lentic-insert-file-local
;;;;;;  lentic-mode lentic-toggle-auto-sync-point lentic-default-init)
;;;;;;  "lentic" "lentic.el" (21656 50222 352759 825000))
;;; Generated autoloads from lentic.el

(autoload 'lentic-default-init "lentic" "\
Default init function.
see `lentic-init' for details.

\(fn)" nil nil)

(autoload 'lentic-toggle-auto-sync-point "lentic" "\


\(fn)" t nil)

(autoload 'lentic-mode "lentic" "\


\(fn &optional ARG)" t nil)

(autoload 'lentic-insert-file-local "lentic" "\


\(fn INIT-FUNCTION)" t nil)

(autoload 'lentic-start-mode "lentic" "\


\(fn &optional ARG)" t nil)

(defvar global-lentic-start-mode nil "\
Non-nil if Global-Lentic-Start mode is enabled.
See the command `global-lentic-start-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-lentic-start-mode'.")

(custom-autoload 'global-lentic-start-mode "lentic" nil)

(autoload 'global-lentic-start-mode "lentic" "\
Toggle Lentic-Start mode in all buffers.
With prefix ARG, enable Global-Lentic-Start mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Lentic-Start mode is enabled in all buffers where
`lentic-start-on' would do it.
See `lentic-start-mode' for more information on Lentic-Start mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (lentic-clojure-latex-delayed-init lentic-latex-clojure-init
;;;;;;  lentic-clojure-latex-init) "lentic-latex-code" "lentic-latex-code.el"
;;;;;;  (21656 50222 343760 243000))
;;; Generated autoloads from lentic-latex-code.el

(autoload 'lentic-clojure-latex-init "lentic-latex-code" "\


\(fn)" nil nil)

(autoload 'lentic-latex-clojure-init "lentic-latex-code" "\


\(fn)" nil nil)

(autoload 'lentic-clojure-latex-delayed-init "lentic-latex-code" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (lentic-el-org-init lentic-org-el-init) "lentic-org"
;;;;;;  "lentic-org.el" (21656 50222 306761 961000))
;;; Generated autoloads from lentic-org.el

(autoload 'lentic-org-el-init "lentic-org" "\


\(fn)" nil nil)

(autoload 'lentic-el-org-init "lentic-org" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("lentic-asciidoc.el" "lentic-block.el"
;;;;;;  "lentic-delayed.el" "lentic-dev.el" "lentic-pkg.el") (21656
;;;;;;  50222 371674 146000))

;;;***

(provide 'lentic-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lentic-autoloads.el ends here
