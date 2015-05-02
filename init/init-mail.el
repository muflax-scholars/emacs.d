;;; mail settings

(require 'smtpmail)
(setq message-send-mail-function	'smtpmail-send-it)
(setq send-mail-function        	'smtpmail-send-it)

(setq smtpmail-smtp-user          	"muflax@gmail.com")
(setq smtpmail-smtp-server        	"smtp.gmail.com")
(setq smtpmail-default-smtp-server	"smtp.gmail.com")
(setq smtpmail-smtp-service       	587)
(setq smtpmail-stream-type        	'starttls)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'init-mail)
