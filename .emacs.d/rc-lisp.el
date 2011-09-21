(defun emacs-lisp-mode-hook ()
	(auto-fill-mode 1)
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-m e") 'eval-last-sexp )
  (set (make-local-variable 'slime-lisp-implementations)
       (list (assoc 'sbcl slime-lisp-implementations))))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hook)

