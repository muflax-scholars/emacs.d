;; useful keyboard macros (that are too unstable / temporarily useful to turn into functions)

;; quick key bindings
(global-set-key (kbd "C-t C-t") 'insert-kbd-macro)
(global-set-key (kbd "C-t C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-t C-b") 'kmacro-bind-to-key)

;; selection of useful macros. this should be smarter / automatic / project-specific, but getting that to work is meh.
(fset 'm\ clear\ column
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 124 13 right right 67108896 19 124 13 left left 23 delete right right] 0 "%d")) arg)))


(provide 'init-macro)
