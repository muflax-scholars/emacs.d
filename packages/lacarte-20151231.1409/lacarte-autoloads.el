;;; lacarte-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "lacarte" "lacarte.el" (22153 15758 934479
;;;;;;  166000))
;;; Generated autoloads from lacarte.el

(let ((loads (get 'lacarte 'custom-loads))) (if (member '"lacarte" loads) nil (put 'lacarte 'custom-loads (cons '"lacarte" loads))))

(defvar lacarte-convert-menu-item-function nil "\
*Function to call to convert a menu item.
Used by `lacarte-execute-menu-command'.  A typical use would be to
remove the `&' characters used in MS Windows menus to define keyboard
accelerators.  See `lacarte-remove-w32-keybd-accelerators'.")

(custom-autoload 'lacarte-convert-menu-item-function "lacarte" t)

(defface lacarte-shortcut '((((background dark)) (:foreground "gray70")) (t (:foreground "gray50"))) "\
*Face used to highlight key binding of menu item `*Completions*'." :group (quote Icicles-Completions-Display) :group (quote faces))

(autoload 'lacarte-execute-command "lacarte" "\
Execute a menu-bar menu command or an ordinary command.
Type a menu item or a command name.  Completion is available.
With a prefix arg, only menu items are available.
Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.

If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.  With Icicles, by default menu items
are sorted before non-menu commands, and menu items are highlighted
using face `icicle-special-candidate'.

\(fn &optional NO-COMMANDS-P)" t nil)

(autoload 'lacarte-execute-menu-command "lacarte" "\
Execute a menu-bar menu command.
Type a menu item.  Completion is available.

A prefix argument controls which menus are available:

* None: current major mode, global, and minor-mode keymaps.
* Positive (including plain `C-u'): current major mode keymap.
* Zero (e.g., `C-0'): current global keymap.
* Negative (e.g., `C--'): current minor mode keymaps.

Completion is not case-sensitive.  However, if you use Icicles, then
you can use `C-A' in the minibuffer to toggle case-sensitivity.
If you use Icicles, then you can also sort the completion candidates
in different ways, using `C-,'.

\(fn MAPS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lacarte-autoloads.el ends here
