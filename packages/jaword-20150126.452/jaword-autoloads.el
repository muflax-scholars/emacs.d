;;; jaword-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-jaword-mode jaword-mode jaword-transpose
;;;;;;  jaword-backward-kill jaword-kill jaword-mark jaword-forward
;;;;;;  jaword-backward) "jaword" "jaword.el" (21702 34914 430934
;;;;;;  178000))
;;; Generated autoloads from jaword.el

(autoload 'jaword-backward "jaword" "\
Like backward-word, but handles Japanese words better.

\(fn ARG)" t nil)

(autoload 'jaword-forward "jaword" "\
Like forward-word, but handle Japanese words better.

\(fn ARG)" t nil)

(put 'jaword 'forward-op 'jaword-forward)

(autoload 'jaword-mark "jaword" "\
Like mark-word, but handle Japanese words better.

\(fn &optional ARG ALLOW-EXTEND)" t nil)

(autoload 'jaword-kill "jaword" "\
Like kill-word, but handle Japanese words better.

\(fn N)" t nil)

(autoload 'jaword-backward-kill "jaword" "\
Like backward-kill-word, but handle Japanese words better.

\(fn N)" t nil)

(autoload 'jaword-transpose "jaword" "\
Like transpose-words, but handle Japanese words better.

\(fn N)" t nil)

(autoload 'jaword-mode "jaword" "\
Toggle Japanese word movement and editing.

\(fn &optional ARG)" t nil)

(defvar global-jaword-mode nil "\
Non-nil if Global-Jaword mode is enabled.
See the command `global-jaword-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-jaword-mode'.")

(custom-autoload 'global-jaword-mode "jaword" nil)

(autoload 'global-jaword-mode "jaword" "\
Toggle Jaword mode in all buffers.
With prefix ARG, enable Global-Jaword mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Jaword mode is enabled in all buffers where
`(lambda nil (jaword-mode 1))' would do it.
See `jaword-mode' for more information on Jaword mode.

\(fn &optional ARG)" t nil)

(defadvice isearch-yank-word-or-char (around jaword-support-isearch activate) "\
Add support for jaword." (if jaword-mode (isearch-yank-internal (lambda nil (if (or (= (char-syntax (or (char-after) 0)) 119) (= (char-syntax (or (char-after (1+ (point))) 0)) 119)) (jaword-forward 1) (forward-char 1)) (point))) ad-do-it))

(defadvice isearch-yank-word (around jaword-support-isearch activate) "\
Add support for jaword." (if jaword-mode (isearch-yank-internal (lambda nil (jaword-forward 1) (point))) ad-do-it))

;;;***

;;;### (autoloads nil nil ("jaword-pkg.el") (21702 34914 448266 725000))

;;;***

(provide 'jaword-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jaword-autoloads.el ends here
