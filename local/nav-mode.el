;;; nav-mode.el ---

;; something of a vim-like modal layer

(defvar nav-mode-map (make-keymap))
(defvar nav/cursor-old-color (face-background 'cursor))

(define-minor-mode nav-mode
  "Something of a vim-like modal navigation layer."
  nil " »" nav-mode-map :global t

  (if nav-mode
      (set-face-background 'cursor "red")
    (set-face-background 'cursor lesson/cursor-old-color)))

(provide 'nav-mode)
;;; nav-mode.el ends here
