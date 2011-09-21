(require 'scala-mode-auto)

(defun scala-turnoff-indent-tabs-mode ()
  (defun scala-indent-line () 
    (interactive)
    (scala-indent-line-to (scala-indentation)))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq indent-tabs-mode nil))

;; scala mode hooks
(add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)
(defun run-scala () (interactive) (scala-run-scala scala-interpreter))