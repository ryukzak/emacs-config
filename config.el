;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Aleksandr Penskoi"
      user-mail-address "aleksandr.penskoi@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Anonymous Pro" :size 14))
(setq all-the-icons-scale-factor 1)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)
(setq doom-modeline-icon nil)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Hotkeys on russian layout
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta) (super))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)

;; Hotkeys
(global-set-key (kbd "s-f") 'counsel-projectile-find-file)
(global-set-key (kbd "s-F") 'counsel-projectile-switch-project)
(global-set-key (kbd "s-r") 'counsel-recentf)

(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'backward-char)
(global-set-key (kbd "s-l") 'forward-char)
(global-set-key (kbd "s-u") 'backward-word)
(global-set-key (kbd "s-o") 'forward-word)
(global-set-key (kbd "s-p") 'recenter)

(global-set-key (kbd "s-J") 'mwim-beginning-of-line)
(global-set-key (kbd "s-L") 'mwim-end-of-line)

(global-set-key (kbd "s-s") 'swiper)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "s-n") 'find-file)

(defun my-workspace ()
  (interactive)
  (delete-other-windows (selected-window))
  (treemacs)
  (winum-select-window-1)
  (split-window-horizontally)
  (winum-select-window-2)
  (split-window-vertically)
  (winum-select-window-3)
  (switch-to-buffer "*compilation*")
  (let ((w (selected-window)))
    (window-resize w (- 24 (window-total-height w))))
  (winum-select-window-1))
(global-set-key (kbd "<f12>") 'my-workspace)

;; Autosave all buffers on focus lost
(setq auto-save-default nil)
(setq create-lockfiles nil)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

;; Compile
(defun save-all-and-compile ()
  (interactive)
  (save-some-buffers 1)
  (let ((default-directory (projectile-ensure-project (projectile-project-root))))
    (compile compile-command)))
(global-set-key (kbd "s-b") 'save-all-and-compile)


(defun save-all-and-compile-by ()
  (interactive)
  (let ((command (compilation-read-command compile-command)))
    (unless (equal command (eval compile-command))
      (setq compile-command command))
    (save-all-and-compile)))
(global-set-key (kbd "s-B") 'save-all-and-compile-by)

;; Haskell
(defun rk-haskell-mode-hook ()
  (toggle-truncate-lines nil)
  (setq haskell-stylish-on-save 't))
(add-hook 'haskell-mode-hook 'rk-haskell-mode-hook)

;; Golang
(defun rk-go-rename-safe-for-windows ()
  (interactive)
  (let ((windows-conf nil))
    (save-buffer)
    (window-configuration-to-register windows-conf)
    (call-interactively 'go-rename)
    (jump-to-register windows-conf)))

(defun rk-go-mode-hook ()
  (local-set-key (kbd "s-6") 'go-rename-safe-for-windows)
  (local-set-key (kbd "s-g") 'dumb-jump-go)
  (local-set-key (kbd "s-G") 'dumb-jump-back))
(add-hook 'go-mode-hook 'rk-go-mode-hook)

;; Spells
(setq exec-path (append exec-path '("/usr/local/bin")))
(package-initialize)
(use-package! guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-langcodes '((en . ("en_US" "English"))
                                   (ru . ("ru-yeyo" "Russian"))))
  :diminish guess-language-mode )


(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))
