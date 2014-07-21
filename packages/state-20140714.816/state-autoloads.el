;;; state-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (state-on state-global-mode state-mode state-define-state)
;;;;;;  "state" "state.el" (21453 26401 280388 886000))
;;; Generated autoloads from state.el

(autoload 'state-define-state "state" "\
Define a new state named NAME with property list ARGS.

:name Symbol representing the state.

:key String of length 1 used as a key in keymap `state-mode-map'
to switch to the state.

:in Field that is used to say if emacs currently displays the
state. If it is a string, return non-nil if current buffer is
visiting a file that is an ancestor of that string. If it is a
form or function, call it.

:switch Field that is used to perform the actual switch. It is
called if it is a function or a form. If it is a valid path,
switch to a buffer visiting that file or switch to the buffer
with that name. If that field is not specified, infer a suitable
one if :in is a string.

:exist Function or form called to say if the state exists. Some
states might require a set up when first called. :exist is used
to say if that set up has already been made.

:create Function or form called to create the state. It is linked
to the :exist property. When the state does not exists, :create
is called.

:before Function or form called just before switching. It allows
the current state to save its state. By default, it saves the
current windows configuration.

:bound Field saying if the current state should only be
accessible from another state. It is the name of another state or
a form to be called.

:priority A number indicating the priority of a state when
several states hold the same key. The state with the lowest
priority is preferred. If several states have the same lowest
priority, ask the user to choose. By convention, nil is of
infinite priority.

:keep A form or function that is called if we keep pressing the
key after switching. Leave nil is you don't want this feature.

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

;;;### (autoloads nil nil ("state-pkg.el") (21453 26401 304958 826000))

;;;***

(provide 'state-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; state-autoloads.el ends here
