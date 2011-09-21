(require 'slime)
(slime-setup)
(eval-after-load "slime"
  '(progn
;;     (slime-setup '(slime-fancy slime-asdf slime-banner slime-fuzzy slime-repl;;     (setq slime-complete-symbol*-fancy t)
;;     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (local-set-key (kbd "M-m p") 'slime-eval-print-last-expression)
     (local-set-key (kbd "M-m e") 'slime-eval-last-expression)
     ))
