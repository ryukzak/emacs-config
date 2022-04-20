;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Aleksandr Penskoi" user-mail-address "aleksandr.penskoi@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font (font-spec :family "Hasklig" :size 13))
(setq doom-font (font-spec :family "Fira Code"
                           :size 13))
(setq all-the-icons-scale-factor 1)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-machine)
(setq doom-modeline-icon nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-indirect-buffer-display 'other-window)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(package-initialize)
(add-to-list 'load-path "/Users/penskoi/.local/bin")

(load! "misc.el")
(load! "kbd.el")

;; Autosave all buffers on focus lost
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(setq dired-omit-files (string-join '("^.DS_Store$" "^.git$" "^\\.$" "^dist-newstyle$") "\\|"))

;; autocomplete
(setq company-idle-delay 0.2 company-minimum-prefix-length 3)

(setenv "PATH" (concat "/Users/penskoi/.local/bin:/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/Users/penskoi/.local/bin")
(add-to-list 'exec-path "/usr/local/bin")


;; Spells
(use-package! guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config (setq guess-language-languages '(en ru) guess-language-langcodes '((en . ("en_US"
                                                                                    "English"))
                                                                             (ru . ("ru-yeyo"
                                                                                    "Russian")))))

(add-hook 'text-mode-hook #'flyspell-mode)

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))


;; Haskell
(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :config (setq ormolu-process-path "fourmolu" ormolu-extra-args nil)
  :bind (:map haskell-mode-map
         ("<f1>" . hoogle)
         ("C-c r" . ormolu-format-buffer)))

(use-package lsp-haskell
  :ensure t
  :config (setq lsp-haskell-server-path
                "/nix/store/bcd9f7801lqvbqzyhimfmczqi1prmcrp-haskell-language-server-exe-haskell-language-server-1.6.1.0/bin/haskell-language-server"
                lsp-log-io t lsp-file-watch-threshold 2500 lsp-haskell-formatting-provider
                "fourmolu" lsp-document-sync-method 'full))

(defun rk-haskell-mode-hook ()
  (push "[/\\\\]gen/" lsp-file-watch-ignored)
  ;; (setq haskell-process-path-ghci "stack exec -- ghci")
  (setq haskell-indentation-layout-offset 4 haskell-indentation-left-offset 4
        haskell-indentation-starter-offset 4 haskell-indentation-where-post-offset 4
        haskell-indentation-where-pre-offset 4 haskell-stylish-on-save nil))
(add-hook 'haskell-mode-hook 'rk-haskell-mode-hook)
(defun rk-inferior-haskell-mode-hook ()
  (setq compilation-first-column 1)
  (setq compilation-error-regexp-alist (cons `("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)?"
                                               1 2 4 ,@(if (fboundp 'compilation-fake-loc)
                                                           '((6)
                                                             nil)))
                                             (cdr (cdr inferior-haskell-error-regexp-alist)))))

(add-hook 'inferior-haskell-mode-hook 'rk-inferior-haskell-mode-hook)


(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

;; Javascript
(use-package web-mode
  :ensure t
  :mode   (("\\.html?\\'" . web-mode)
           ("\\.css\\'"   . web-mode)
           ("\\.jsx\\'"  . web-mode)
           ("\\.tsx\\'"  . web-mode)
           ("\\.json\\'"  . web-mode))
  :init (progn
          (defun rk-web-mode-hook ()
            (setq-default web-mode-comment-formats (remove '("javascript" . "/*")
                                                           web-mode-comment-formats))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
            (add-to-list 'web-mode-comment-formats '("tsx" . "//")))
          (add-hook 'web-mode-hook 'rk-web-mode-hook)))

(use-package prettier-js
  :ensure t
  :init (progn (add-hook 'web-mode-hook 'prettier-js-mode)))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(defun rk-markdown-mode-hook ()
  (local-set-key (kbd "s-1") 'markdown-insert-header-atx-1)
  (local-set-key (kbd "s-2") 'markdown-insert-header-atx-2)
  (local-set-key (kbd "s-3") 'markdown-insert-header-atx-3)
  (local-set-key (kbd "s-4") 'markdown-insert-header-atx-4)
  (local-set-key (kbd "s-5") 'markdown-insert-header-atx-5))

(use-package markdown-mode
  :ensure t
  :config (add-hook 'markdown-mode-hook 'rk-markdown-mode-hook))

(use-package! ox-leanpub
  :after org)

;; Verilog
(use-package verilog-mode
  :ensure t
  :init (progn
          (setq verilog-tool 'verilog-linter verilog-linter "iverilog" verilog-coverage "iverilog"
                verilog-simulator "vvp" verilog-compiler "iverilog" verilog-auto-newline nil
                verilog-tab-always-indent nil verilog-auto-indent-on-newline t verilog-indent-level
                4 verilog-case-indent 4 verilog-indent-level-module 0
                verilog-indent-level-declaration 4 verilog-indent-level-behavioral  4
                verilog-indent-level-directive 4)))

;; org-mode
(defun rk-org-mode-hook ()
  (auto-fill-mode 0)
  (visual-line-mode t)
  (require 'ob-python)
  (org-babel-do-load-languages 'org-babel-load-languages '((python . t)
                                                           (forth . t)
                                                           (emacs-lisp . t)))
  (local-set-key (kbd "s-0") 'org-tree-to-indirect-buffer)
  (local-set-key (kbd "s-9") 'org-publish-all)
  (local-set-key (kbd "C-q") 'org-unfill-paragraph))

(add-hook 'org-mode-hook 'rk-org-mode-hook)

(use-package ox-reveal
  :ensure t
  :init (progn
          (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))

(defun save-and-cider-eval-buffer ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer))

(use-package cider
  :ensure t
  :bind (("<f12>" . cider-format-buffer)
         ("C-c l l" . save-and-cider-eval-buffer)))

(use-package lsp-docker
  :ensure t)
