(require 'objc-c-mode)

(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))

(add-to-list 'ac-modes 'objc-mode)

(defun my-objc-mode-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq comment-column 40
        tab-width 4
        c-basic-offset tab-width)
  (auto-complete-mode 't)
  ;; (paredit-mode 't)
  )
(add-hook 'objc-mode-hook 'my-objc-mode-hook)
