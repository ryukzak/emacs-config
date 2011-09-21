(require 'clojure-mode)

(defun my-clojure-mode-hook ()
  (hs-minor-mode 1)
  (paredit-mode 1)
  (local-set-key (kbd "M-m p") 'slime-eval-print-last-expression)
  (local-set-key (kbd "M-m e") 'slime-eval-last-expression)
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\>\\)"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "ƒ")
                              nil))))))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-protocol-version 'ignore)
