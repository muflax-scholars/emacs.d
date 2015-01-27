;;; lentic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (lentic-default-init) "lentic" "lentic.el" (21703
;;;;;;  47560 109996 724000))
;;; Generated autoloads from lentic.el

(autoload 'lentic-default-init "lentic" "\
Default init function.
see `lentic-init' for details.

\(fn)" nil nil)

;;;***

;;;### (autoloads (lentic-asciidoc-clojure-init) "lentic-asciidoc"
;;;;;;  "lentic-asciidoc.el" (21703 47560 52999 462000))
;;; Generated autoloads from lentic-asciidoc.el

(autoload 'lentic-asciidoc-clojure-init "lentic-asciidoc" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (lentic-delayed-default-init lentic-delayed-init)
;;;;;;  "lentic-delayed" "lentic-delayed.el" (21703 47560 21000 999000))
;;; Generated autoloads from lentic-delayed.el

(autoload 'lentic-delayed-init "lentic-delayed" "\


\(fn DELAYED)" nil nil)

(autoload 'lentic-delayed-default-init "lentic-delayed" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (lentic-dev-enable-insertion-marking lentic-dev-random-face
;;;;;;  lentic-dev-reinit lentic-dev-post-command-hook lentic-dev-after-change-function)
;;;;;;  "lentic-dev" "lentic-dev.el" (21703 47560 7001 671000))
;;; Generated autoloads from lentic-dev.el

(autoload 'lentic-dev-after-change-function "lentic-dev" "\
Run the change functions out of the command loop.
Using this function is the easiest way to test an new
`lentic-clone' method, as doing so in the command loop is
painful for debugging. Set variable `lentic-emergency' to
true to disable command loop functionality.

\(fn)" t nil)

(autoload 'lentic-dev-post-command-hook "lentic-dev" "\
Run the post-command functions out of the command loop.
Using this function is the easiest way to test an new
`lentic-convert' method, as doing so in the command loop is
painful for debugging. Set variable `lentic-emergency' to
true to disable command loop functionality.

\(fn)" t nil)

(autoload 'lentic-dev-reinit "lentic-dev" "\
Recall the init function regardless of current status.
This can help if you have change the config object and need
to make sure there is a new one.

\(fn)" t nil)

(autoload 'lentic-dev-random-face "lentic-dev" "\
Change the insertion face to a random one.

\(fn)" t nil)

(autoload 'lentic-dev-enable-insertion-marking "lentic-dev" "\
Enable font locking properties for inserted text.

\(fn)" t nil)

;;;***

;;;### (autoloads (lentic-doc-external-view lentic-doc-eww-view lentic-doc-generate-self)
;;;;;;  "lentic-doc" "lentic-doc.el" (21703 47559 998002 103000))
;;; Generated autoloads from lentic-doc.el

(autoload 'lentic-doc-generate-self "lentic-doc" "\


\(fn)" t nil)

(autoload 'lentic-doc-eww-view "lentic-doc" "\


\(fn)" t nil)

(autoload 'lentic-doc-external-view "lentic-doc" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (lentic-haskell-latex-init lentic-clojure-latex-delayed-init
;;;;;;  lentic-latex-clojure-init lentic-clojure-latex-init) "lentic-latex-code"
;;;;;;  "lentic-latex-code.el" (21703 47560 101997 108000))
;;; Generated autoloads from lentic-latex-code.el

(autoload 'lentic-clojure-latex-init "lentic-latex-code" "\


\(fn)" nil nil)

(autoload 'lentic-latex-clojure-init "lentic-latex-code" "\


\(fn)" nil nil)

(autoload 'lentic-clojure-latex-delayed-init "lentic-latex-code" "\


\(fn)" nil nil)

(autoload 'lentic-haskell-latex-init "lentic-latex-code" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads (global-lentic-start-mode lentic-start-mode lentic-mode-insert-file-local
;;;;;;  lentic-mode lentic-mode-toggle-auto-sync-point) "lentic-mode"
;;;;;;  "lentic-mode.el" (21703 47560 92997 541000))
;;; Generated autoloads from lentic-mode.el

(autoload 'lentic-mode-toggle-auto-sync-point "lentic-mode" "\


\(fn)" t nil)

(autoload 'lentic-mode "lentic-mode" "\


\(fn &optional ARG)" t nil)

(easy-menu-change '("Edit") "Lentic" '(["Create Here" lentic-mode-create-in-selected-window :active (not lentic-config)] ["Split Below" lentic-mode-split-window-below :active (not lentic-config)] ["Split Right" lentic-mode-split-window-right :active (not lentic-config)] ["Insert File Local" lentic-mode-insert-file-local :active (not lentic-config)] ["Move Here" lentic-mode-move-lentic-window :active lentic-config] ["Swap" lentic-mode-swap-lentic-window :active lentic-config] ["Read Doc (eww)" lentic-doc-eww-view] ["Read Doc (external)" lentic-doc-external-view]))

(autoload 'lentic-mode-insert-file-local "lentic-mode" "\


\(fn INIT-FUNCTION)" t nil)

(autoload 'lentic-start-mode "lentic-mode" "\


\(fn &optional ARG)" t nil)

(defvar global-lentic-start-mode nil "\
Non-nil if Global-Lentic-Start mode is enabled.
See the command `global-lentic-start-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-lentic-start-mode'.")

(custom-autoload 'global-lentic-start-mode "lentic-mode" nil)

(autoload 'global-lentic-start-mode "lentic-mode" "\
Toggle Lentic-Start mode in all buffers.
With prefix ARG, enable Global-Lentic-Start mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Lentic-Start mode is enabled in all buffers where
`lentic-start-on' would do it.
See `lentic-start-mode' for more information on Lentic-Start mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (lentic-orgel-org-init lentic-org-orgel-init lentic-el-org-init
;;;;;;  lentic-org-el-init) "lentic-org" "lentic-org.el" (21703 47560
;;;;;;  33000 423000))
;;; Generated autoloads from lentic-org.el

(autoload 'lentic-org-el-init "lentic-org" "\


\(fn)" nil nil)

(autoload 'lentic-el-org-init "lentic-org" "\


\(fn)" nil nil)

(autoload 'lentic-org-orgel-init "lentic-org" "\


\(fn)" nil nil)

(autoload 'lentic-orgel-org-init "lentic-org" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("build.el" "lentic-block.el" "lentic-pkg.el"
;;;;;;  "noisy-change.el") (21703 47560 130511 59000))

;;;***

(provide 'lentic-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lentic-autoloads.el ends here