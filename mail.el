;; -*- lexical-binding: t; -*-

(defun my-make-mu4e-context (context-name full-name mail-address)
  (let ((dir-name ""))
    (make-mu4e-context
     :name context-name
     :vars
     `((user-mail-address    . ,mail-address)
       (user-full-name       . ,full-name)
       (mu4e-sent-folder     . ,(concat dir-name "/Sent Items"))
       (mu4e-drafts-folder   . ,(concat dir-name "/Drafts"))
       (mu4e-trash-folder    . ,(concat dir-name "/Deleted Items"))
       (mu4e-refile-folder   . ,(concat dir-name "/Archive"))
       ))))

;;Fixing duplicate UID errors when using mbsync and mu4e
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-get-mail-command "mbsync -a") 

(setq mu4e-contexts
      `(,(my-make-mu4e-context
	  "ETH"
	  "Mohammad Reza Karimi"
	  "mkarimi@inf.ethz.ch")))
