;;; racket-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (run-racket racket-mode) "racket-mode" "racket-mode.el"
;;;;;;  (21183 23570 142356 757000))
;;; Generated autoloads from racket-mode.el

(setq auto-mode-alist (append '(("\\.rkt\\'" . racket-mode) ("\\.rktd\\'" . racket-mode)) auto-mode-alist))

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket.
\\{racket-mode-map}

\(fn)" t nil)

(autoload 'run-racket "racket-mode" "\
Run an inferior Racket process, input and output via buffer `*racket*'.
If there is a process already running in `*racket*', switch to that buffer.
Runs the hook `inferior-racket-mode-hook' (after the `comint-mode-hook'
is run).

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("racket-mode-pkg.el") (21183 23570 162136
;;;;;;  604000))

;;;***

(provide 'racket-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here
