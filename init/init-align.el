;; aligning things

;; elastic tab stops
(require 'elastic-tabstops)

(setq elastic-tab-align-modes
      '(
        enh-ruby-mode
        notes-mode
        nix-mode
        emacs-lisp-mode
        lisp-mode
        racket-mode
        scheme-mode
        sh-mode
        js2-mode
        rust-mode
        c-mode
        c++-mode
        ))

(elastic-advice-command       	indent-for-tab-command)
(elastic-advice-command       	literal-tab)
(elastic-advice-command       	indent-according-to-mode)
(elastic-advice-command-region	indent-region)
(elastic-advice-command       	comment-dwim)
(elastic-advice-command-region	comment-region)
(elastic-advice-command-region	uncomment-region)

(add-hook 'notes-mode-hook 'elastic-turn-on-extended-columns)

(provide 'init-align)
