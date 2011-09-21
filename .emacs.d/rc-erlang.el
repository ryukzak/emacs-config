(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(setq erlang-root-dir "/usr/lib/erlang")
(add-to-list 'exec-path "/usr/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/lib/erlang/man")

(require 'distel)
(distel-setup)

(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))

  (flymake-mode)
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  (local-set-key [return] 'newline-and-indent)
	(local-set-key "\C-hf" 'erlang-man-function))

(setq inferior-erlang-machine-options '("-sname" "emacs"))


(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
;; (add-hook 'my-erlang-mode-hook 'common-hook)
;; (add-hook 'my-erlang-mode-hook 'show-prog-keywords)

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; (add-to-list 'load-path "~/.emacs.d/elisp/esense")
;; (require 'esense-start)
;; (setq esense-indexer-program "~/.emacs.d/elisp/esense/esense.sh")

;;; emacs-rc-erlang.el ends here
