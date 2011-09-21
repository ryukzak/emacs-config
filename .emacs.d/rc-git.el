;; (push "~/.emacs.d/elisp/git" load-path)
;; (require 'git)

(require 'magit)
(global-set-key (kbd "M-m g s") 'magit-status)
(global-set-key (kbd "M-m g >") 'magit-push)
(global-set-key (kbd "M-m g <") 'magit-pull)
(custom-set-variables
 '(magit-save-some-buffers (quote dontask))
 '(magit-git-executable "/usr/bin/git"))

(define-key magit-mode-map (kbd "o") 'magit-toggle-file-section)


;; (push "~/.emacs.d/elisp/egg" load-path)
;; (require 'egg)
;; (global-set-key "\C-cm" 'egg-status)
