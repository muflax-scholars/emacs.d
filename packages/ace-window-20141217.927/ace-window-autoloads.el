;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ace-window ace-swap-window ace-delete-window ace-select-window)
;;;;;;  "ace-window" "ace-window.el" (21656 50246 69658 245000))
;;; Generated autoloads from ace-window.el

(autoload 'ace-select-window "ace-window" "\
Ace select window.

\(fn)" t nil)

(autoload 'ace-delete-window "ace-window" "\
Ace delete window.

\(fn)" t nil)

(autoload 'ace-swap-window "ace-window" "\
Ace swap window.

\(fn)" t nil)

(autoload 'ace-window "ace-window" "\
Select a window with `ace-jump-mode'and perform an action based on prefix ARG.
Variations are described below.

By default, behaves like extended `other-window'.

Prefixed with one \\[universal-argument], does a swap between selected window
 and current window, so that the selected buffer moves to current window (and
 current buffer moves to selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected window.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ace-window-pkg.el") (21656 50246 103916
;;;;;;  374000))

;;;***

(provide 'ace-window-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-window-autoloads.el ends here
