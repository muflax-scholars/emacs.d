;;; psci-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inferior-psci-mode psci/quit! psci/reset! psci/load-project-modules!
;;;;;;  psci/load-module! psci/load-current-file! psci-mode psci)
;;;;;;  "psci" "psci.el" (21648 50082 424370 689000))
;;; Generated autoloads from psci.el

(autoload 'psci "psci" "\
Run an inferior instance of `psci' inside Emacs.

\(fn)" t nil)

(autoload 'psci-mode "psci" "\
Major mode for `run-psci'.

\\<psci-mode-map>

\(fn)" t nil)

(autoload 'psci/load-current-file! "psci" "\
Load the current file in the psci repl.

\(fn)" t nil)

(autoload 'psci/load-module! "psci" "\
Load the module inside the repl session.

\(fn)" t nil)

(autoload 'psci/load-project-modules! "psci" "\
Load the modules needed for the repl session.
We chose to load the .psci file's content (the purescript doc proposes its use).

\(fn)" t nil)

(autoload 'psci/reset! "psci" "\
Reset the current status of the repl session.

\(fn)" t nil)

(autoload 'psci/quit! "psci" "\
Quit the psci session.

\(fn)" t nil)

(autoload 'inferior-psci-mode "psci" "\
psci minor mode to define default bindings.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("psci-dev.el" "psci-pkg.el") (21648 50082
;;;;;;  432631 623000))

;;;***

(provide 'psci-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; psci-autoloads.el ends here
