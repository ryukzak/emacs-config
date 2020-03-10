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
(defun rk-org-mode-hook ()
  (local-set-key (kbd "s-0") 'org-tree-to-indirect-buffer)
  (local-set-key (kbd "C-q") 'org-unfill-paragraph))
(add-hook 'org-mode-hook 'rk-org-mode-hook)
(setq org-indirect-buffer-display 'other-window)

(defun org-unfill-paragraph (&optional region)
  "Unfill the region, joining text paragraphs into a single
    logical line. This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (org-fill-paragraph nil region)))



;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
;(define-key map (kbd "r")        #'treemacs-visit-node-in-most-recently-used-window)

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

(package-initialize)

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

(setq company-idle-delay 0.2
      company-minimum-prefix-length 2)

;; Hotkeys
(global-set-key (kbd "s-f") 'counsel-projectile-find-file)
(global-set-key (kbd "s-F") 'counsel-projectile-switch-project)
(global-set-key (kbd "s-r") 'counsel-recentf)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'backward-char)
(global-set-key (kbd "s-l") 'forward-char)
(global-set-key (kbd "s-u") 'backward-word)
(global-set-key (kbd "s-o") 'forward-word)
(global-set-key (kbd "s-p") 'recenter)
(global-set-key (kbd "s-d") 'mc/mark-next-like-this)

(global-set-key (kbd "s-J") 'mwim-beginning-of-line)
(global-set-key (kbd "s-L") 'mwim-end-of-line)

(global-set-key (kbd "s-s") 'swiper)
(global-set-key (kbd "C-s") 'counsel-projectile-ag)
(global-set-key (kbd "s-n") 'find-file)

(global-set-key (kbd "s-Z") 'undo-tree-redo)

(defun my-workspace ()
  (interactive)
  (when (and (boundp 'treemacs-get-local-window) (treemacs-get-local-window))
    (delete-window (treemacs-get-local-window)))
  (delete-other-windows (selected-window))
  (split-window-horizontally)
  (other-window 1)
  (split-window-vertically (- (window-total-height (selected-window)) 24))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (unless (boundp 'treemacs--init) (treemacs))
  (treemacs--init)
  (other-window 1))
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
  (toggle-truncate-lines t)
  (dante-mode)
  (setq xref-backend-functions '(etags--xref-backend t)
        company-idle-delay 0.2
        company-minimum-prefix-length 3
        haskell-process-type 'cabal-new-repl
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-where-post-offset 4
        haskell-indentation-where-pre-offset 4
        haskell-tags-on-save t
        haskell-stylish-on-save t))
(add-hook 'haskell-mode-hook 'rk-haskell-mode-hook)

;; Golang
(use-package! go-rename
  :ensure t)

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
(add-hook 'before-save-hook #'gofmt-before-save)

;; Spells
(setq exec-path (append exec-path '("/usr/local/bin")))
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

(after! treemacs
  (setq treemacs-width 24)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (treemacs-define-doubleclick-action 'file-node-open #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-define-doubleclick-action 'file-node-close #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-define-doubleclick-action 'tag-node #'treemacs-visit-node-in-most-recently-used-window)
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)))


;; Javascript
(use-package prettier-js
  :ensure t
  :init (progn
          (add-hook 'js2-mode-hook #'prettier-js-mode)
          (add-hook 'web-mode-hook #'prettier-js-mode)
          (add-hook 'typescript-mode-hook #'prettier-js-mode)
          (add-hook 'web-mode-hook #'(lambda ()
                                       (enable-minor-mode
                                        '("\\.jsx?\\'" . prettier-js-mode))))))

;; Verilog
(use-package verilog-mode
  :ensure t
  :init (progn
          (setq verilog-tool 'verilog-linter
                verilog-linter "iverilog"
                verilog-coverage "iverilog"
                verilog-simulator "vvp"
                verilog-compiler "iverilog"
                verilog-auto-newline nil
                verilog-tab-always-indent nil
                verilog-auto-indent-on-newline t
                verilog-indent-level 4
                verilog-case-indent 4
                verilog-indent-level-module 0
                verilog-indent-level-declaration 4
                verilog-indent-level-behavioral  4
                verilog-indent-level-directive 4)))
