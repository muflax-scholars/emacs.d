;; useful keyboard macros (that are too unstable / temporarily useful to turn into functions)

;; quick key bindings
(global-set-key (kbd "C-t C-t") 'insert-kbd-macro)
(global-set-key (kbd "C-t C-n") 'kmacro-name-last-macro)
(global-set-key (kbd "C-t C-b") 'kmacro-bind-to-key)

;; selection of useful macros. this should be smarter / automatic / project-specific, but getting that to work is meh.
(fset 'm\ clear\ column
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([18 124 13 right right 67108896 19 124 13 left left 23 delete right right] 0 "%d")) arg)))
(global-set-key (kbd "C-t c") 'm\ clear\ column)

(fset 'm\ add\ annotation
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return 62 32] 0 "%d")) arg)))
(global-set-key (kbd "C-t a") 'm\ add\ annotation)

(fset 'm\ add\ explanation
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 return 60 32] 0 "%d")) arg)))
(global-set-key (kbd "C-t e") 'm\ add\ explanation)

(fset 'm\ turn\ into\ example
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 125 13 right 11 backspace backspace 46 1 33 32 delete 134217848 116 105 116 108 101 99 97 115 return] 0 "%d")) arg)))
(global-set-key [20 105] 'm\ turn\ into\ example)


(provide 'init-macro)
