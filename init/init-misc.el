;; random options

;; calendar
(setq calendar-week-start-day 1)	; monday
(setq calendar-date-style 't)   	; sanity

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; make sure we always know what's happening when eval-ing things
(setq eval-expression-print-level nil)

(setq safe-local-variable-values
      '(
        ;; basic encoding header
        (encoding . utf-8)
        ;; formatting
        (truncate-lines 1)
        (hl-line-mode 1)
        ;; zsh files
        (eval sh-set-shell "zsh")
        ))


(provide 'init-misc)
