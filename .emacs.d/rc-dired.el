(require 'tramp)

(when (require 'dired-sync nil t)
   (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync)
   (global-set-key (kbd "M-m s s") 'dired-do-sync)
   (global-set-key (kbd "M-m s >") 'dired-do-sync-push)
   (global-set-key (kbd "M-m s <") 'dired-do-sync-pull))

(setq tramp-default-method "ssh")

(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files "")
(setq dired-omit-files
      (concat dired-omit-files "^\\.[^.].*$\\|^\\.$"))
