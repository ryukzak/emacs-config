(add-to-list 'load-path "~/.emacs.d/el-get/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(setq yasnippet-dir "~/.emacs.d/snippets")
(yas/load-directory yasnippet-dir)

;; hook for automatic reloading of changed snippets
(defun my-update-yasnippets-on-save ()
  (when (string-match "/.emacs.d/snippets" buffer-file-name)
    (yas/load-directory yasnippet-dir)))
(add-hook 'after-save-hook 'my-update-yasnippets-on-save)
