;;; ~/.doom.d/kbd.el -*- lexical-binding: t; -*-

;; Hotkeys on russian layout
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method
           (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control)
                         (meta)
                         (control meta)
                         (super))))
    (when input-method (activate-input-method input-method))
    (when (and current-input-method
               quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation (cadr map)
                                            (char-to-string to) 1)))
          (when (and (characterp from)
                     (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method (activate-input-method current))))

(reverse-input-method 'russian-computer)

(defun my-workspace ()
  (interactive)
  (when (and (boundp 'treemacs-get-local-window)
             (treemacs-get-local-window))
    (delete-window (treemacs-get-local-window)))
  (delete-other-windows (selected-window))
  (split-window-horizontally)
  (other-window 1)
  (split-window-vertically (- (window-total-height (selected-window)) 24))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (unless (boundp 'treemacs--init)
    (treemacs))
  (treemacs--init)
  (other-window 1))
(global-set-key (kbd "<f12>") 'my-workspace)

(global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
;; (global-set-key (kbd "s-D") 'mc/mark-next-like-this)

;; Hotkeys
;; (global-set-key (kbd "C-y") 'counsel-projectile-find-file)
;; (global-set-key (kbd "C-Y") 'counsel-projectile-switch-project)
;; (global-set-key (kbd "C-c e") 'counsel-recentf)


;; Super key duplicate
;; (global-set-key (kbd "s-w") 'kill-this-buffer)

;; (global-set-key (kbd "s-i") 'previous-line)
;; (global-set-key (kbd "s-k") 'next-line)
;; (global-set-key (kbd "s-j") 'backward-char)
;; (global-set-key (kbd "s-l") 'forward-char)

;; (global-set-key (kbd "s-K") 'scroll-up-command)
;; (global-set-key (kbd "s-I") 'scroll-down-command)
;; (global-set-key (kbd "s-h") 'move-beginning-of-line)
;; (global-set-key (kbd "s-H") 'move-end-of-line)

;; (global-set-key (kbd "s-u") 'backward-word)
;; (global-set-key (kbd "s-o") 'forward-word)
;; (global-set-key (kbd "s-U") 'backward-paragraph)
;; (global-set-key (kbd "s-O") 'forward-paragraph)

;; (global-set-key (kbd "s-p") 'recenter)
;; (global-set-key (kbd "s-P") 'counsel-git)
;; (global-set-key (kbd "C-~") 'shell)
;; ;; (global-set-key (kbd "s-d") 'mc/mark-next-like-this)


;; (global-set-key (kbd "s-e") 'backward-kill-word)
;; (global-set-key (kbd "s-r") 'kill-word)
;; (global-set-key (kbd "s-d") 'delete-backward-char)
;; (global-set-key (kbd "s-f") 'delete-forward-char)
;; (global-set-key (kbd "s-F") 'rg-dwim)

;; (global-set-key (kbd "s-0") 'paredit-forward-slurp-sexp)
;; (global-set-key (kbd "s-)") 'paredit-forward-barf-sexp)

;; (global-set-key (kbd "s-9") 'paredit-backward-slurp-sexp)
;; (global-set-key (kbd "s-(") 'paredit-backward-barf-sexp)

(defun run-smerge ()
  (interactive)
  (shell-command "smerge ."))

(global-set-key (kbd "<f11>") 'run-smerge)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

;; (global-set-key (kbd "s-g") 'kill-line)
;; (global-set-key (kbd "s-G") 'backward-kill-line)

;; (global-set-key (kbd "s-J") 'beginning-of-buffer)
;; (global-set-key (kbd "s-L") 'end-of-buffer)

;; (global-set-key (kbd "s-s") '+default/search-buffer)
;; (global-set-key (kbd "C-s") 'counsel-projectile-ag)
;; (global-set-key (kbd "s-n") 'find-file)

;; (global-set-key (kbd "s-x") 'kill-region)
;; (global-set-key (kbd "s-z") 'undo)
;; (global-set-key (kbd "s-Z") 'redo)
;; (global-set-key (kbd "S-SPC") 'set-mark-command)

(setq projectile-sort-order 'recentf)

(defun save-all-and-recompile ()
  (interactive)
  (save-all-and-compile))

(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (call-interactively #'projectile-compile-project))

(setq compilation-window-height nil)
(setq compilation-scroll-output t)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'meta))

(paredit-mode 1)

(when (or 't (eq system-type 'gnu/linux))
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  (ergoemacs-mode 1)

  ;; (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-o") 'consult-find)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-o") 'projectile-find-file)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-M-o") 'consult-recent-file)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-x C-f") 'ergoemacs-find-file)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-~") 'run-smerge)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-`") 'magit-status)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-<escape> M-<escape>") 'run-smerge)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-M-a") 'eshell)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-b") '+vertico/switch-workspace-buffer)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-B") 'ido-switch-buffer)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-b") 'save-all-and-recompile)
  ;; (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-c b") 'save-all-and-compile)

  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-f") '+default/search-buffer)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-F") 'rg-dwim)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-M-f") '+default/search-project)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-'") 'comment-line)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M->") 'lsp-find-references)

  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-J") 'paredit-backward)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-L") 'paredit-forward)

  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-]") 'paredit-forward-slurp-sexp)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-}") 'paredit-forward-barf-sexp)

  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-[") 'paredit-backward-slurp-sexp)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-{") 'paredit-backward-barf-sexp)

  ;; (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-b") 'cider-user-reload)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-6") 'cider-switch-to-last-clojure-buffer)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-7") 'cider-eval-last-sexp)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-8") 'cider-pprint-eval-last-sexp)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-9") 'cider-test-run-test)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M--") 'cider-format-region)

  ;; (global-unset-key (kbd "<C-mouse-4>") )
  ;; (global-unset-key (kbd "<C-mouse-5>"))


  (defun clean-term-artifacts ()
    "e.g. arrow keys, super mod"
    (interactive)
    (when (eq (line-number-at-pos (point))
              (line-number-at-pos (point-max)))
      (replace-regexp-in-region "[ABCD]+$" "" (line-beginning-position)))
    (while (re-search-backward "^[ABCD]$" nil t)
      (delete-char 1))
    (replace-regexp-in-region "[[:digit:]]*;[[:digit:]]u" "" (line-beginning-position)
                              (line-end-position))
    (message "term artifacts cleaned"))

  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-p") 'clean-term-artifacts)
  ;; (ergoemacs-define-key ergoemacs-user-keymap (kbd "<f8>") 'clean-term-artifacts)

  (xterm-mouse-mode))
