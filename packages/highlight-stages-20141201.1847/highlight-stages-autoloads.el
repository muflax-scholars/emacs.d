;;; highlight-stages-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (highlight-stages-global-mode highlight-stages-mode)
;;;;;;  "highlight-stages" "highlight-stages.el" (21637 50054 285517
;;;;;;  599000))
;;; Generated autoloads from highlight-stages.el

(autoload 'highlight-stages-mode "highlight-stages" "\
Highlight staged (quasi-quoted) expressions

\(fn &optional ARG)" t nil)

(defvar highlight-stages-global-mode nil "\
Non-nil if Highlight-Stages-Global mode is enabled.
See the command `highlight-stages-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `highlight-stages-global-mode'.")

(custom-autoload 'highlight-stages-global-mode "highlight-stages" nil)

(autoload 'highlight-stages-global-mode "highlight-stages" "\
Toggle Highlight-Stages mode in all buffers.
With prefix ARG, enable Highlight-Stages-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Stages mode is enabled in all buffers where
`(lambda nil (highlight-stages-mode 1))' would do it.
See `highlight-stages-mode' for more information on Highlight-Stages mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("highlight-stages-pkg.el") (21637 50054
;;;;;;  299076 643000))

;;;***

(provide 'highlight-stages-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-stages-autoloads.el ends here
