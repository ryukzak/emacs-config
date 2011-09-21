(setq load-path (cons "~/.emacs.d/elisp/org-7.5/lisp" load-path))
(require 'org-install)

(setq calendar-week-start-day 1)

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files
      (map 'cons (lambda (x) (concat org-directory "/" x))
           '("todo.org")))

(setq org-mobile-files
      (map 'cons (lambda (x) (concat org-directory "/" x))
           '("todo.org" "maybe.org" "inbox.org")))

(setq org-agenda-use-time-grid 't)
(setq org-agenda-time-grid '((daily today require-timed)
                             "----------------"
                             (1000 1200 1400 1600 1800 2000)))

(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-include-all-todo 't)
(global-set-key "\C-cpush" 'org-mobile-push)
(global-set-key "\C-cpull" 'org-mobile-pull)

;; (run-with-timer 5 (* 60 30)
;;                 (lambda nil (org-mobile-pull)
;;                   (org-mobile-push)))