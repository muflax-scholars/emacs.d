;;; racket-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "racket-edit" "racket-edit.el" (21463 27939
;;;;;;  223034 490000))
;;; Generated autoloads from racket-edit.el

(add-to-list 'hs-special-modes-alist '(racket-mode "(" ")" ";" nil nil))

;;;***

;;;### (autoloads (racket-mode) "racket-mode" "racket-mode.el" (21463
;;;;;;  27939 374027 517000))
;;; Generated autoloads from racket-mode.el

(autoload 'racket-mode "racket-mode" "\
Major mode for editing Racket.
\\{racket-mode-map}

\(fn)" t nil)

(setq auto-mode-alist (append '(("\\.rkt\\'" . racket-mode) ("\\.rktd\\'" . racket-mode)) auto-mode-alist))

;;;***

;;;### (autoloads (racket-repl) "racket-repl" "racket-repl.el" (21463
;;;;;;  27939 343028 949000))
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
;;;;;;  "racket-tests.el") (21463 27939 406363 224000))

;;;***

(provide 'racket-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; racket-mode-autoloads.el ends here