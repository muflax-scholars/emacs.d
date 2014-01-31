;;; easy-kill-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (easy-mark easy-kill) "easy-kill" "easy-kill.el"
;;;;;;  (21227 26960 684616 6000))
;;; Generated autoloads from easy-kill.el

(autoload 'easy-kill "easy-kill" "\
Kill thing at point in the order of region, url, email and line.
Temporally activate additional key bindings as follows:

  letters => select or expand selection according to `easy-kill-alist';
  0..9    => expand selection by that number;
  +,=/-   => expand or shrink selection;
  @       => append selection to previous kill;
  C-w     => kill selection;
  C-SPC   => turn selection into an active region;
  C-g     => abort;
  others  => save selection and exit.

\(fn &optional N)" t nil)

(defalias 'easy-mark-sexp 'easy-mark "\
Use `easy-mark' instead. The alias may be removed in future.")

(autoload 'easy-mark "easy-kill" "\
Similar to `easy-kill' (which see) but for marking.

\(fn &optional N)" t nil)

;;;***

;;;### (autoloads nil nil ("easy-kill-pkg.el") (21227 26960 703666
;;;;;;  27000))

;;;***

(provide 'easy-kill-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; easy-kill-autoloads.el ends here
