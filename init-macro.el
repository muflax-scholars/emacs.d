;; useful keyboard macros (that are too unstable / temporarily useful to turn into functions)

;; quick key bindings
(global-set-key (kbd "C-t C-t") 'insert-kbd-macro)
(global-set-key (kbd "C-t C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-t C-b") 'kmacro-bind-to-key)

;; selection of useful macros. this should be smarter / automatic / project-specific, but getting that to work is meh.
(fset 'm\ clear\ column
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 124 13 right right 67108896 19 124 13 left left 23 delete right right] 0 "%d")) arg)))

(fset 'm\ add\ annotation
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return 62 32] 0 "%d")) arg)))
(global-set-key [20 97] 'm\ add\ annotation)

(fset 'm\ add\ explanation
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return 60 32] 0 "%d")) arg)))
(global-set-key [20 101] 'm\ add\ explanation)

(fset 'm\ next\ empty\ \?
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 63 32 36 13 134217848 114 101 99 101 110 116 101 114 return] 0 "%d")) arg)))
(global-set-key [20 110] 'm\ add\ explanation)
(global-set-key [f13] 'm\ next\ empty\ \?)


(provide 'init-macro)
