;;; ace-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ace-window ace-swap-window ace-delete-window ace-select-window
;;;;;;  aw-generic aw-list-visual-area) "ace-window" "ace-window.el"
;;;;;;  (21623 48432 59855 701000))
;;; Generated autoloads from ace-window.el

(autoload 'aw-list-visual-area "ace-window" "\
Forward to `ace-jump-list-visual-area', removing invisible frames.

\(fn)" nil nil)

(autoload 'aw-generic "ace-window" "\
Create a window-manipulating function.
MODE-LINE is a string to display while a window is being selected.
HANDLER is a function that takes a window argument.

\(fn MODE-LINE HANDLER)" nil t)

(autoload 'ace-select-window "ace-window" "\


\(fn)" t nil)

(autoload 'ace-delete-window "ace-window" "\


\(fn)" t nil)

(autoload 'ace-swap-window "ace-window" "\


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

;;;### (autoloads nil nil ("ace-window-pkg.el") (21623 48432 79199
;;;;;;  844000))

;;;***

(provide 'ace-window-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-window-autoloads.el ends here
