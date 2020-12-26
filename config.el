;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Aleksandr Penskoi"
      user-mail-address "aleksandr.penskoi@gmail.com")

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
(setq doom-font (font-spec :family "Hasklig" :size 13))
(setq all-the-icons-scale-factor 1)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-modeline-icon nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-indirect-buffer-display 'other-window)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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


(setq dired-omit-files
      "^\\.?#\\|^\\.$\\|^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\|git\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")


;; autocomplete
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)


;; Spells
(use-package! guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-langcodes '((en . ("en_US" "English"))
                                   (ru . ("ru-yeyo" "Russian"))))
  )

(add-hook 'text-mode-hook #'flyspell-mode)

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))


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
(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :config
  (setq ormolu-process-path "fourmolu")
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer)))

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-server-path (concat (getenv "HOME") "/.local/bin/haskell-language-server-wrapper")
        lsp-log-io t
        lsp-haskell-formatting-provider "fourmolu"
        lsp-document-sync-method 'full))

(defun rk-haskell-mode-hook ()
  (push "[/\\\\]gen/" lsp-file-watch-ignored)
  (setq
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-starter-offset 4
        haskell-indentation-where-post-offset 4
        haskell-indentation-where-pre-offset 4
        haskell-stylish-on-save nil))
(add-hook 'haskell-mode-hook 'rk-haskell-mode-hook)


;; Javascript
(use-package prettier-js
  :ensure t
  :init (progn
          (add-hook 'js2-mode-hook 'prettier-js-mode)
          (add-hook 'typescript-mode-hook 'prettier-js-mode)
          (add-hook 'scss-mode-hook 'prettier-js-mode)
          (add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


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


;; org-mode
(defun rk-org-mode-hook ()
  (auto-fill-mode 0)
  (visual-line-mode t)
  (local-set-key (kbd "s-0") 'org-tree-to-indirect-buffer)
  (local-set-key (kbd "s-9") 'org-publish-all)
  (local-set-key (kbd "C-q") 'org-unfill-paragraph))

(defun org-latex-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to LaTeX.
    CONTENTS holds the contents of the block.  INFO is a plist
    holding contextual information."
  (org-latex--wrap-label
   quote-block
   (format "\\begin{leftbar}\n%s\\end{leftbar}" contents)))

(add-hook 'org-mode-hook 'rk-org-mode-hook)

(use-package ox-reveal
  :ensure t
  :init (progn (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))
