;;; paxedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (paxedit-mode paxedit-transpose-backward paxedit-transpose-forward
;;;;;;  paxedit-delete paxedit-copy paxedit-kill paxedit-context-goto-definition
;;;;;;  paxedit-context-new-statement paxedit-backward-end-2 paxedit-backward-up-2
;;;;;;  paxedit-backward-end paxedit-backward-up paxedit-sexp-close-newline
;;;;;;  paxedit-function-goto-definition paxedit-sexp-close-statement
;;;;;;  paxedit-macro-expand-replace paxedit-wrap-comment paxedit-sexp-raise
;;;;;;  paxedit-close-sexp-newline-round paxedit-close-sexp-newline
;;;;;;  paxedit-quoted-open-round paxedit-sexp-backward-end paxedit-sexp-backward-up
;;;;;;  paxedit-comment-align-all paxedit-previous-symbol paxedit-next-symbol
;;;;;;  paxedit-symbol-occur paxedit-symbol-change-case paxedit-symbol-kill
;;;;;;  paxedit-symbol-copy paxedit-goto-end-of-symbol paxedit-goto-start-of-symbol
;;;;;;  paxedit-cleanup paxedit-indent-buffer paxedit-untabify-buffer
;;;;;;  paxedit-delete-whitespace paxedit-whitespace-delete-right
;;;;;;  paxedit-whitespace-delete-left) "paxedit" "paxedit.el" (21637
;;;;;;  50042 777054 25000))
;;; Generated autoloads from paxedit.el

(autoload 'paxedit-whitespace-delete-left "paxedit" "\
Delete all the whitespace on the left side until a non-space character is encountered.

\(fn)" t nil)

(autoload 'paxedit-whitespace-delete-right "paxedit" "\
Delete all the whitespace on the right side until a non-space character is encountered.

\(fn)" t nil)

(autoload 'paxedit-delete-whitespace "paxedit" "\
Delete all whitespace to the left and right of the cursor.

\(fn)" t nil)

(autoload 'paxedit-untabify-buffer "paxedit" "\
Remove all tabs in the buffer.

\(fn)" t nil)

(autoload 'paxedit-indent-buffer "paxedit" "\
Re-indent buffer.

\(fn)" t nil)

(autoload 'paxedit-cleanup "paxedit" "\
Indent buffer as defined by mode, remove tabs, and delete trialing whitespace.

\(fn)" t nil)

(autoload 'paxedit-goto-start-of-symbol "paxedit" "\
Go to the start of the current symbol.

\(fn)" t nil)

(autoload 'paxedit-goto-end-of-symbol "paxedit" "\
Go to the end of the current symbol.

\(fn)" t nil)

(autoload 'paxedit-symbol-copy "paxedit" "\
Copy the symbol the cursor is on or next to.

\(fn)" t nil)

(autoload 'paxedit-symbol-kill "paxedit" "\
Kill the symbol the text cursor is next to or in and cleans up the left-over whitespace from kill.

\(fn)" t nil)

(autoload 'paxedit-symbol-change-case "paxedit" "\
Change the symbol to all uppercase if any of the symbol characters are lowercase, else lowercase the whole symbol.

\(fn)" t nil)

(autoload 'paxedit-symbol-occur "paxedit" "\
Search for symbol the cursor is on or next to in the current buffer with occur.

\(fn)" t nil)

(autoload 'paxedit-next-symbol "paxedit" "\
Go to the next symbol.

\(fn &optional N)" t nil)

(autoload 'paxedit-previous-symbol "paxedit" "\
Go to the previous symbol.

\(fn &optional N)" t nil)

(autoload 'paxedit-comment-align-all "paxedit" "\
Align all the comments from the point of the cursor onwards.

\(fn)" t nil)

(autoload 'paxedit-sexp-backward-up "paxedit" "\
Go to the start of the containing parent expression.

\(fn N)" t nil)

(autoload 'paxedit-sexp-backward-end "paxedit" "\
Go to the end of the containing parent expression.

\(fn &optional N)" t nil)

(autoload 'paxedit-quoted-open-round "paxedit" "\
Insert quoted open round.

\(fn)" t nil)

(autoload 'paxedit-close-sexp-newline "paxedit" "\
Close current round and newline. Faster version of the default paredit close round and newline procedure.

\(fn)" t nil)

(autoload 'paxedit-close-sexp-newline-round "paxedit" "\
Close the current expression, create a newline, and create a new parenthesis pair.

\(fn)" t nil)

(autoload 'paxedit-sexp-raise "paxedit" "\
Raises the expression the cursor is in while perserving the cursor location.

\(fn)" t nil)

(autoload 'paxedit-wrap-comment "paxedit" "\
Wrap a comment macro around the current expression. If the current expression is already wrapped by a comment, then the wrapping comment is removed.

\(fn)" t nil)

(autoload 'paxedit-macro-expand-replace "paxedit" "\
Replace the current expression (if there is a macro in the functional position) with its macro expansion.

\(fn)" t nil)

(autoload 'paxedit-sexp-close-statement "paxedit" "\
Faster version of the default paredit close round and newline procedure.

\(fn)" t nil)

(autoload 'paxedit-function-goto-definition "paxedit" "\
Split the current window and display the definition of the function.

\(fn)" t nil)

(autoload 'paxedit-sexp-close-newline "paxedit" "\
Faster version of the default paredit close round and newline procedure.

\(fn)" t nil)

(autoload 'paxedit-backward-up "paxedit" "\
Move to the start of the explicit expression, implicit expression or comment.

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-end "paxedit" "\
Move to the end of the explicit expression, implicit expression or comment.

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-up-2 "paxedit" "\
Go up expressions by multiples of two and place cursor at start of context.

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-end-2 "paxedit" "\
Go up expressions by multiples of two and place cursor at end of context.

\(fn &optional N)" t nil)

(autoload 'paxedit-context-new-statement "paxedit" "\
Create a new SEXP depending on the context.

\(fn &optional N)" t nil)

(autoload 'paxedit-context-goto-definition "paxedit" "\
Go to the function definition.

\(fn)" t nil)

(autoload 'paxedit-kill "paxedit" "\
Kill current explicit expression, implicit expression, or comment. Also cleans up left-over whitespace from kill and corrects indentation.

\(fn &optional N)" t nil)

(autoload 'paxedit-copy "paxedit" "\
Copy current explicit expression, implicit expression, or comment.

\(fn &optional N)" t nil)

(autoload 'paxedit-delete "paxedit" "\
Delete current explicit expression, implicit expression, or comment. Also cleans up the left-over whitespace from deletion and corrects indentation.

\(fn &optional N)" t nil)

(autoload 'paxedit-transpose-forward "paxedit" "\
Swap the current explicit expression, implicit expression, symbol, or comment forward depending on what the cursor is on and what is available to swap with. This command is very versatile and will do the \"right\" thing in each context.

\(fn &optional N)" t nil)

(autoload 'paxedit-transpose-backward "paxedit" "\
Swaps the current explicit, implicit expression, symbol, or comment backward depending on what the cursor is on and what is available to swap with. Swaps in the opposite direction of paxedit-transpose-forward.

\(fn &optional N)" t nil)

(autoload 'paxedit-mode "paxedit" "\
Mode to enable Paxedit functionality and maintain keybindings.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("paxedit-pkg.el") (21637 50042 809998
;;;;;;  729000))

;;;***

(provide 'paxedit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; paxedit-autoloads.el ends here
