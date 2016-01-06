;; aligning things

;; elastic tab stops
(require 'elastic-tabstops)

(setq elastic-tab-align-modes
      '(
        prog-mode
        text-mode
        notes-mode
        ))

(elastic-advice-command       	indent-for-tab-command)
(elastic-advice-command       	literal-tab)
(elastic-advice-command       	indent-according-to-mode)
(elastic-advice-command-region	indent-region)
(elastic-advice-command       	comment-dwim)
(elastic-advice-command-region	comment-region)
(elastic-advice-command-region	uncomment-region)

(load-after 'lispy
  (elastic-advice-command	lispy-comment))

(add-hook 'notes-mode-hook 'elastic-turn-on-extended-columns)

(provide 'init-align)
