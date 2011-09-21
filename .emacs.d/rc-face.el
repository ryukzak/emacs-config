;; highlight select
(transient-mark-mode 't)

;; frendly scroll
(global-hl-line-mode 't)
(setq scroll-step 1)

(setq default-buffer-file-coding-system 'utf-8)
(set-language-environment 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

;; normal C-k behaviour
(setq kill-whole-line 't)

(setq visible-bell 't)

;; don't ask, what buffer need to kill
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; ;; time on status bar
(setq display-time-interval 1)
(setq display-time-format "%H:%M")
(display-time-mode)

(custom-set-variables
 '(column-number-mode 't)
 '(custom-file nil)
 '(imenu-auto-rescan 't)
 '(indent-tabs-mode nil)
 '(show-paren-mode 't)
 '(tab-width 2)
 '(transient-mark-mode 't)
 '(scalable-fonts-allowed t))

(menu-bar-mode nil)
;; (scroll-bar-mode nil)
(tool-bar-mode nil)

