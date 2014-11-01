(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
	  starttls-use-gnutls t
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials
	  '(("smtp.gmail.com" 587 "lwx169@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587)

;; something about myself
(setq
 user-mail-address "lwx169@gmail.com"
 user-full-name  "Li wenxiang")

(require 'mu4e)
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")
(setq mu4e-maildir       "~/Mail/")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
(setq mu4e-html2text-command "w3m -dump -T text/html")

(provide 'mail-cfg)
