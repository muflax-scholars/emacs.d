;;; state-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (state-on state-global-mode state-mode state-define-state)
;;;;;;  "state" "state.el" (21419 40106 425954 861000))
;;; Generated autoloads from state.el

(autoload 'state-define-state "state" "\
Define a new state named NAME with property list ARGS.

\(fn NAME &rest ARGS)" nil t)

(autoload 'state-mode "state" "\
Minor mode to switch between workspaces.

\(fn &optional ARG)" t nil)

(defvar state-global-mode nil "\
Non-nil if State-Global mode is enabled.
See the command `state-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `state-global-mode'.")

(custom-autoload 'state-global-mode "state" nil)

(autoload 'state-global-mode "state" "\
Toggle State mode in all buffers.
With prefix ARG, enable State-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

State mode is enabled in all buffers where
`state-on' would do it.
See `state-mode' for more information on State mode.

\(fn &optional ARG)" t nil)

(autoload 'state-on "state" "\
Enable State minor mode.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("state-pkg.el") (21419 40106 467798 150000))

;;;***

(provide 'state-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; state-autoloads.el ends here
