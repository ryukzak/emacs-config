(if (equal user-login-name "ryukzak")
    (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv")
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us"))
;; (require 'ergoemacs-mode)
(push "~/.emacs.d/el-get/ergoemacs-keybindings" load-path)
(load "~/.emacs.d/el-get/ergoemacs-keybindings/ergoemacs-mode")

(ergoemacs-mode 't)
(setq x-super-keysym 'meta)
(setq x-alt-keysym 'meta)

(require 'mwheel)
(mwheel-install)

(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 
											 (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key (kbd "M-m F") 'mac-toggle-max-window)

(global-set-key (kbd "M-m M-e") 'eshell)
(global-set-key (kbd "M-m c") 'compile)
(global-set-key (kbd "M-m b") 'ido-switch-buffer)
(global-set-key (kbd "M-m o") 'speedbar-get-focus)
(global-set-key (kbd "M-m q") 'fill-paragraph)
(global-set-key (kbd "M-m 9") 'kmacro-start-macro)
(global-set-key (kbd "M-m 0") 'kmacro-end-macro)
(global-set-key (kbd "M-m r") 'kmacro-call-macro)

(setq x-select-enable-clipboard t)

;; (when (require 'follow-mouse)
;;   (setq follow-mouse-deselect-active-minibuffer nil)
;;   (turn-on-follow-mouse))

;; (global-set-key "\C-c\C-i" 'overwrite-mode) 
;; (global-set-key "\C-xX" 'previous-buffer)
;; (global-set-key "\C-xx" 'next-buffer)
;; (global-set-key (kbd "<XF86Back>") 'previous-buffer)
;; (global-set-key (kbd "<XF86Forward>") 'next-buffer)

;; (global-set-key "\C-w" 'backward-kill-word)
;; (global-set-key "\C-x\C-k" 'kill-region)
;; (global-set-key "\C-xS" 'eshell)
;; (global-set-key	"\C-xC" 'compile)	
;; (global-set-key "\C-cc" 'comment-region)

;; (global-set-key "\C-x\C-b" 'bs-show)

;; (global-set-key "\C-x\C-k" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)
;; (global-set-key "\C-y" 'clipboard-yank)

;; (global-set-key "\M-gm" 'imenu)

;; (global-set-key (kbd "<S-lefttab>") 'hs-toggle-hiding)
;; (setq default-input-method "russian-computer")

