(require 'escreen)

(when (require 'workspace)
  (workspace-create)
  (global-set-key (kbd "M-m m") 'workspace-controller)
  (define-key workspace-controller-map (kbd "M-n") 'workspace-next-workspace)
  (define-key workspace-controller-map (kbd "n") 'workspace-create)
  (define-key workspace-controller-map (kbd "M-p") 'workspace-previous-workspace))