;;; string-inflection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "string-inflection" "string-inflection.el"
;;;;;;  (21974 10527 998278 608000))
;;; Generated autoloads from string-inflection.el

(autoload 'string-inflection-ruby-style-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar

\(fn)" t nil)

(autoload 'string-inflection-java-style-cycle "string-inflection" "\
fooBar => FOO_BAR => FooBar => fooBar

\(fn)" t nil)

(autoload 'string-inflection-all-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => fooBar => foo_bar

\(fn)" t nil)

(autoload 'string-inflection-toggle "string-inflection" "\
toggle foo_bar <=> FooBar

\(fn)" t nil)

(autoload 'string-inflection-camelcase "string-inflection" "\
FooBar format

\(fn)" t nil)

(autoload 'string-inflection-lower-camelcase "string-inflection" "\
fooBar format

\(fn)" t nil)

(autoload 'string-inflection-underscore "string-inflection" "\
foo_bar format

\(fn)" t nil)

(autoload 'string-inflection-upcase "string-inflection" "\
FOO_BAR format

\(fn)" t nil)

(autoload 'string-inflection-lisp "string-inflection" "\
foo-bar format

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; string-inflection-autoloads.el ends here
