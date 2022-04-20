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
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "s-D") 'mc/mark-next-like-this)

;; Hotkeys
;; (global-set-key (kbd "C-y") 'counsel-projectile-find-file)
;; (global-set-key (kbd "C-Y") 'counsel-projectile-switch-project)
;; (global-set-key (kbd "C-c e") 'counsel-recentf)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'backward-char)
(global-set-key (kbd "s-l") 'forward-char)

(global-set-key (kbd "s-K") 'scroll-up-command)
(global-set-key (kbd "s-I") 'scroll-down-command)
(global-set-key (kbd "s-h") 'move-beginning-of-line)
(global-set-key (kbd "s-H") 'move-end-of-line)

(global-set-key (kbd "s-u") 'backward-word)
(global-set-key (kbd "s-o") 'forward-word)
(global-set-key (kbd "s-U") 'backward-paragraph)
(global-set-key (kbd "s-O") 'forward-paragraph)

(global-set-key (kbd "s-p") 'recenter)
(global-set-key (kbd "s-P") 'counsel-git)
(global-set-key (kbd "C-~") 'shell)
;; (global-set-key (kbd "s-d") 'mc/mark-next-like-this)


(global-set-key (kbd "s-e") 'backward-kill-word)
(global-set-key (kbd "s-r") 'kill-word)
(global-set-key (kbd "s-d") 'delete-backward-char)
(global-set-key (kbd "s-f") 'delete-forward-char)
(global-set-key (kbd "s-F") 'counsel-projectile-rg)

(global-set-key (kbd "s-0") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "s-)") 'paredit-forward-barf-sexp)

(global-set-key (kbd "s-9") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "s-(") 'paredit-backward-barf-sexp)

(defun run-smerge ()
  (interactive)
  (shell-command "smerge ."))

(global-set-key (kbd "<f11>") 'run-smerge)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key (kbd "s-g") 'kill-line)
(global-set-key (kbd "s-G") 'backward-kill-line)

(global-set-key (kbd "s-J") 'beginning-of-buffer)
(global-set-key (kbd "s-L") 'end-of-buffer)

(global-set-key (kbd "s-s") 'swiper)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "s-n") 'find-file)

(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'redo)
(global-set-key (kbd "S-SPC") 'set-mark-command)

(setq projectile-sort-order 'recentf)

;; Compile
(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (let ((default-directory (projectile-ensure-project (projectile-project-root))))
    (compile compile-command)))

(defun save-all-and-compile-by ()
  (interactive)
  (save-some-buffers 1)
  (let ((default-directory (projectile-ensure-project (projectile-project-root))))
    (counsel-compile)))

(setq compilation-scroll-output t)

(when (eq system-type 'gnu/linux)
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  (ergoemacs-mode 1)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-o") 'counsel-projectile-find-file)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-M-o") 'counsel-buffer-or-recentf)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-x C-f") 'ergoemacs-find-file)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-`") 'magit)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-b") 'counsel-switch-buffer)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-c b") 'save-all-and-compile)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-c C-b") 'save-all-and-compile-by)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-f") 'swiper)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "C-M-f") 'projectile-ripgrep)
  (xterm-mouse-mode))
