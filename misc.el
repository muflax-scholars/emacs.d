;; random options

;; calendar
(setq calendar-week-start-day 1)  ; monday
(setq european-calendar-style 't) ; sanity

;; custom variables because fuck emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; make sure we always know what's happening when eval-ing things
(setq eval-expression-print-level nil)

(provide 'misc)
