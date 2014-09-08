;;; hide-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (hide-region-unhide hide-region-hide) "hide-region"
;;;;;;  "hide-region.el" (21517 60888 720952 670000))
;;; Generated autoloads from hide-region.el

(autoload 'hide-region-hide "hide-region" "\
Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\"

\(fn)" t nil)

(autoload 'hide-region-unhide "hide-region" "\
Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\".

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("hide-region-pkg.el") (21517 60888 733957
;;;;;;  768000))

;;;***

(provide 'hide-region-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hide-region-autoloads.el ends here
