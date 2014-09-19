;;; mail settings

(setup "mu4e"
  (setq mu4e-maildir "~/mail")

  (setq mu4e-drafts-folder "/[Google Mail].Drafts")
  (setq mu4e-sent-folder   "/[Google Mail].Sent Mail")
  (setq mu4e-trash-folder  "/[Google Mail].Trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"                  	. ?i)
          ("/[Google Mail].Sent Mail"	. ?s)
          ("/[Google Mail].Trash"    	. ?t)
          ("/[Google Mail].All Mail" 	. ?a)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap"))

(setup "smtpmail"
  (setq message-send-mail-function	'smtpmail-send-it)
  (setq send-mail-function        	'smtpmail-send-it)

  (setq smtpmail-smtp-user          	"muflax@gmail.com")
  (setq smtpmail-smtp-server        	"smtp.gmail.com")
  (setq smtpmail-default-smtp-server	"smtp.gmail.com")
  (setq smtpmail-smtp-service       	587)
  (setq smtpmail-stream-type        	'starttls)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t))

(provide 'init-mail)
