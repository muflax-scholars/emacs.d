;;; mc-extras-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (mc/compare-chars-backward mc/compare-chars-forward
;;;;;;  mc/compare-chars) "mc-compare" "mc-compare.el" (21703 47364
;;;;;;  28417 178000))
;;; Generated autoloads from mc-compare.el

(autoload 'mc/compare-chars "mc-compare" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
With an optional argument, move backwards by calling `mc/compare-chars-backward'.
This command pushes the mark before moving cursors.

\(fn &optional ARG)" t nil)

(autoload 'mc/compare-chars-forward "mc-compare" "\
Compare the character at point with that at each fake cursor, and move forward as far as they all match.
This command pushes the mark before moving cursors.

\(fn)" t nil)

(autoload 'mc/compare-chars-backward "mc-compare" "\
Backwards version of `mc/compare-chars-forward'.

\(fn)" t nil)

;;;***

;;;### (autoloads (mc/cua-rectangle-setup mc/cua-rectangle-to-multiple-cursors)
;;;;;;  "mc-cua" "mc-cua.el" (21703 47364 418 523000))
;;; Generated autoloads from mc-cua.el

(autoload 'mc/cua-rectangle-to-multiple-cursors "mc-cua" "\
Turn CUA rectangle mode into multiple-cursors mode, keeping insert positions and selections.

\(fn)" t nil)

(autoload 'mc/cua-rectangle-setup "mc-cua" "\
Enable interaction between multiple cursors and CUA rectangle copy & paste.

\(fn)" nil nil)

;;;***

;;;### (autoloads (mc/rect-rectangle-to-multiple-cursors) "mc-rect"
;;;;;;  "mc-rect.el" (21703 47364 62415 544000))
;;; Generated autoloads from mc-rect.el

(autoload 'mc/rect-rectangle-to-multiple-cursors "mc-rect" "\
Turn rectangle-mark-mode into multiple-cursors mode, keeping selections.

\(fn START END)" t nil)

;;;***

;;;### (autoloads (mc/remove-duplicated-cursors mc/remove-current-cursor)
;;;;;;  "mc-remove" "mc-remove.el" (21703 47364 55415 880000))
;;; Generated autoloads from mc-remove.el

(autoload 'mc/remove-current-cursor "mc-remove" "\
Remove the current cursor by replacing the next fake cursor with the real cursor.

\(fn)" t nil)

(autoload 'mc/remove-duplicated-cursors "mc-remove" "\
Remove duplicated fake cursors, including ones that overlap the real cursor.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("mc-extras-pkg.el" "mc-extras.el") (21703
;;;;;;  47364 73061 232000))

;;;***

(provide 'mc-extras-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mc-extras-autoloads.el ends here
