(require 'auto-complete)

(define-key ac-completing-map (kbd "M-t") 'ac-next)
(define-key ac-completing-map (kbd "M-c") 'ac-previous)
(setq ac-ignore-case nil)
