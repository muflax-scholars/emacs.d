;;; elmacro-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elmacro-mode elmacro-show-lossage elmacro-show-last-macro)
;;;;;;  "elmacro" "elmacro.el" (21504 35295 919058 6000))
;;; Generated autoloads from elmacro.el

(autoload 'elmacro-show-last-macro "elmacro" "\
Show the last macro as elisp with NAME.

\(fn NAME)" t nil)

(autoload 'elmacro-show-lossage "elmacro" "\
Show lossage as elisp.

\(fn)" t nil)

(defvar elmacro-mode nil "\
Non-nil if elmacro mode is enabled.
See the command `elmacro-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `elmacro-mode'.")

(custom-autoload 'elmacro-mode "elmacro" nil)

(autoload 'elmacro-mode "elmacro" "\
Toggle elmacro mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("elmacro-pkg.el") (21504 35295 937018
;;;;;;  321000))

;;;***

(provide 'elmacro-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elmacro-autoloads.el ends here
