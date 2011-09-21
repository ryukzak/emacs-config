(add-to-list 'load-path "~/.emacs.d/elisp/mew") 

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))








(setq mew-name "Ryukzak Neskazov") ;; (user-full-name)
(setq mew-user "ryukzak") ;; (user-login-name)
(setq mew-mail-domain "gmail.com")
;; Set using IMAP
(setq mew-proto "%")
(setq mew-imap-server "imap.gmail.com") ;; if not localhost
(setq mew-imap-ssl t)
(setq mew-imap-ssl-port "993")
(setq mew-imap-user "ryukzak@gmail.com")
;; send email via SMTP
(setq mew-smtp-server "smtp.gmail.com") ;; if not localhost
(setq mew-smtp-ssl t)
(setq mew-smtp-ssl-port "587")
(setq mew-smtp-port "587")
(setq mew-smtp-user "ryukzak@gmail.com")
(setq mew-ssl-verify-level 0)
(setq mew-tls-smtp "smtp")
(setq mew-smtp-auth-list '("PLAIN" "LOGIN" "CRAM-MD5"))
;; cache the password
(setq mew-use-cached-passwd t) ;; so I won’t have to type the password all the time.