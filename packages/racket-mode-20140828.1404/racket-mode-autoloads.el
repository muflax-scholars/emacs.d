;;; racket-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "racket-edit" "racket-edit.el" (21504 35254
;;;;;;  667908 759000))
;;; Generated autoloads from racket-edit.el

(add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil))

;;;***

;;;### (autoloads (racket-mode) "racket-mode" "racket-mode.el" (21504
;;;;;;  35254 770904 138000))
;;; Generated autoloads from racket-mode.el

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket.
\\{racket-mode-map}

\(fn)" t nil)

(setq auto-mode-alist (append '(("\\.rkt\\'" . racket-mode) ("\\.rktd\\'" . racket-mode)) auto-mode-alist))

;;;***

;;;### (autoloads (racket-repl) "racket-repl" "racket-repl.el" (21504
;;;;;;  35254 753904 901000))
;;; Generated autoloads from racket-repl.el

(autoload 'racket-repl "racket-repl" "\
Run a Racket REPL in a comint buffer.
Runs the hook `racket-repl-mode-hook' (after the `comint-mode-hook'
is run).

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("racket-common.el" "racket-complete.el"
;;;;;;  "racket-emacs-compat.el" "racket-eval.el" "racket-font-lock.el"
;;;;;;  "racket-indent.el" "racket-keywords-and-builtins.el" "racket-mode-pkg.el"
;;;;;;  "racket-tests.el") (21504 35254 824503 934000))

;;;***

(provide 'racket-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here
