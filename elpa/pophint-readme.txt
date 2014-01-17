This extension provides navigation like the Vimperator Hint Mode of Firefox.
The interface has the following flow.
 1. pop-up tip about the matched point for some action which user want.
 2. do some action for the user selecting.

For more infomation, see <https://github.com/aki2o/emacs-pophint/blob/master/README.md>

Dependencies:

- popup.el ( bundled auto-complete.el. see <https://github.com/auto-complete/auto-complete> )
- yaxception.el ( see <https://github.com/aki2o/yaxception> )
- log4e.el ( see <https://github.com/aki2o/log4e> )

Installation:

Put this to your load-path.
And put the following lines in your .emacs or site-start.el file.

(require 'pophint)

Configuration:

Key Binding
(define-key global-map (kbd "C-;") 'pophint:do-flexibly)
(define-key global-map (kbd "C-+") 'pophint:do)
(define-key global-map (kbd "M-;") 'pophint:redo)
(define-key global-map (kbd "C-M-;") 'pophint:do-interactively)

Customization:

[EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "pophint:" :docstring t)
`pophint:popup-chars'
Characters for the pop-up hint.
`pophint:switch-source-char'
Character for switching using source.
`pophint:switch-direction-char'
Character for switching direction of pop-up.
`pophint:switch-window-char'
Character for switching window of pop-up.
`pophint:popup-max-tips'
Maximum counts of the pop-up hint.
`pophint:default-require-length'
Default minimum length of matched text for pop-up.
`pophint:switch-direction-p'
Whether switch direction of pop-up.
`pophint:do-allwindow-p'
Whether do pop-up at all windows.

 *** END auto-documentation
