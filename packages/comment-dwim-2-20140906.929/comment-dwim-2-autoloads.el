;;; comment-dwim-2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (comment-dwim-2) "comment-dwim-2" "comment-dwim-2.el"
;;;;;;  (21526 16200 165949 78000))
;;; Generated autoloads from comment-dwim-2.el

(autoload 'comment-dwim-2 "comment-dwim-2" "\
Call a comment command according to the context.
If the region is active, call `comment-or-uncomment-region' to
toggle comments.
Else, the function applies to the current line and calls a
different function at each successive call. If the line is not
commented, the behavior is:
comment line -> add end-of-line comment -> restore initial state.
If the line is already commented, uncomment it first.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("comment-dwim-2-pkg.el") (21526 16200
;;;;;;  181563 871000))

;;;***

(provide 'comment-dwim-2-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; comment-dwim-2-autoloads.el ends here
