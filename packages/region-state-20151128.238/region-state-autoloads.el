;;; region-state-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "region-state" "region-state.el" (22124 16269
;;;;;;  564500 602000))
;;; Generated autoloads from region-state.el

(defvar region-state-mode nil "\
Non-nil if Region-State mode is enabled.
See the command `region-state-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `region-state-mode'.")

(custom-autoload 'region-state-mode "region-state" nil)

(autoload 'region-state-mode "region-state" "\
Toggle show the region (aka. selection) state.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; region-state-autoloads.el ends here
