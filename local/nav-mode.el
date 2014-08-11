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
(define-key nav-mode-map (kbd "i") 'backward-sentence)
(define-key nav-mode-map (kbd "a") 'forward-sentence)
(define-key nav-mode-map (kbd "u") 'backward-word)
(define-key nav-mode-map (kbd "e") 'forward-word)

;; forward-button 	forward-char 	forward-comment
;; forward-line 	forward-list 	forward-page
;; forward-paragraph 	forward-point 	forward-same-syntax
;; forward-sentence 	forward-sexp 	forward-symbol
;; forward-thing 	forward-to-indentation 	forward-visible-line
;; forward-whitespace 	forward-word

(define-key nav-mode-map (kbd "p") 'scroll-up-command)
(define-key nav-mode-map (kbd "|") 'scroll-down-command)

(define-key nav-mode-map (kbd "o") 'other-window)
(define-key nav-mode-map (kbd "n") 'other-window)
(define-key nav-mode-map (kbd "r") (lambda () (interactive) (other-window -1)))

;; editing
(define-key nav-mode-map (kbd "c") 'kill-and-join-forward)
(define-key nav-mode-map (kbd "w") 'whole-line-or-region-kill-region)
(define-key nav-mode-map (kbd "y") 'yank-and-indent)

;; exit with space
(defun turn-off-nav-mode ()
  "Turns off nav-mode."
  (interactive)
  (nav-minor-mode 0)
  (nav-global-mode 0))

(define-key nav-mode-map (kbd "SPC") 'turn-off-nav-mode)

(provide 'nav-mode)
;;; nav-mode.el ends here
