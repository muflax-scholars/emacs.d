;;; nav-mode.el ---

;; something of a vim-like modal navigation layer

(defvar nav-mode-map (make-keymap))
(define-minor-mode nav-minor-mode
  "Something of a vim-like modal navigation layer. (buffer-local)"
  nil " >>" nav-mode-map)
(define-minor-mode nav-global-mode
  "Something of a vim-like modal navigation layer. (global)"
  nil " »»" nav-mode-map :global t)

;; simple navigation
(define-key nav-mode-map (kbd "l") 'previous-line)
(define-key nav-mode-map (kbd "a") 'next-line)
(define-key nav-mode-map (kbd "v") 'scroll-down-command)
(define-key nav-mode-map (kbd "c") 'scroll-up-command)
(define-key nav-mode-map (kbd "i") 'left-char)
(define-key nav-mode-map (kbd "e") 'right-char)

;; exit with space
(defun turn-off-nav-mode ()
  "Turns off nav-mode."
  (interactive)
  (nav-minor-mode 0)
  (nav-global-mode 0))

(define-key nav-mode-map (kbd "SPC") 'turn-off-nav-mode)

(provide 'nav-mode)
;;; nav-mode.el ends here
