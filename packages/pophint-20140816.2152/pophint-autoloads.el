;;; pophint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pophint:toggle-use-pos-tip pophint:redo pophint:do-situationally
;;;;;;  pophint:do-interactively pophint:do-flexibly pophint:do)
;;;;;;  "pophint" "pophint.el" (21494 50040 852539 803000))
;;; Generated autoloads from pophint.el

(autoload 'pophint:do "pophint" "\
Do pop-up hint-tip using given source on target to direction.

SOURCE is alist or symbol of alist. About its value, see `pophint:defsource'.
 If nil, its value is the first of SOURCES or `pophint--default-source'.
 If non-nil, `pophint--default-source' isn't used for SOURCES.
SOURCES is list of SOURCE. If this length more than 1, enable switching SOURCE when pop-up hint.
ACTION is function. About this, see action of SOURCE for `pophint:defsource'. If nil, it's used.
ACTION-NAME is string. About this, see name of `pophint:defaction'.
DIRECTION is symbol. The allowed value is the following.
 - forward  ... seek the pop-up point moving forward until `pophint:popup-max-tips'.
 - backward ... seek the pop-up point moving backward until `pophint:popup-max-tips'.
 - around   ... seek the pop-up point moving both until half of `pophint:popup-max-tips'.
 If nil, enable switching DIRECTION when pop-up hint.
NOT-HIGHLIGHT is t or nil. If non-nil, don't highlight matched text when pop-up hint.
WINDOW is window. find next point of pop-up in the window. If nil, its value is `selected-window'.
NOT-SWITCH-WINDOW is t or nil. If non-nil, disable switching window when select shown hint.
ALLWINDOW is t or nil. If non-nil, pop-up at all windows in frame.
USE-POS-TIP is t or nil. If omitted, inherit `pophint:use-pos-tip'.

\(fn &key SOURCE SOURCES ACTION ACTION-NAME DIRECTION NOT-HIGHLIGHT WINDOW NOT-SWITCH-WINDOW ALLWINDOW (use-pos-tip (quote global)))" t nil)

(autoload 'pophint:do-flexibly "pophint" "\
Do pop-up hint-tip using source in `pophint:sources'.

For detail, see `pophint:do'.

\(fn &key ACTION ACTION-NAME WINDOW)" t nil)

(autoload 'pophint:do-interactively "pophint" "\
Do pop-up hint-tip asking about what to do after select hint-tip.

\(fn)" t nil)

(autoload 'pophint:do-situationally "pophint" "\
Do pop-up hint-tip for SITUATION.

SITUATION is symbol used for finding active sources from `pophint:dedicated-sources'.

\(fn SITUATION)" t nil)

(autoload 'pophint:redo "pophint" "\
Redo last pop-up hint-tip using any sources.

\(fn)" t nil)

(autoload 'pophint:toggle-use-pos-tip "pophint" "\
Toggle the status of `pophint:use-pos-tip'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pophint-config.el" "pophint-pkg.el")
;;;;;;  (21494 50040 869882 757000))

;;;***

(provide 'pophint-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pophint-autoloads.el ends here
