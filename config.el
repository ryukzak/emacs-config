;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Aleksandr Penskoi"
      user-mail-address "aleksandr.penskoi@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "FiraCode Nerd Font"
                           :size 12))
(quote
 (set-face-attribute 'default nil :height 120)
 (set-face-attribute 'default nil :height 160)
 )

(setq all-the-icons-scale-factor 1)

(setq centaur-tabs-height 10)
(setq centaur-tabs-set-bar 'left)

(setq centaur-tabs-plain-icons t)
;; (setq x-underline-at-descent-line t)


(setq warning-minimum-level :error)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(package-initialize)
(add-to-list 'load-path "~/.local/bin")
(add-to-list 'load-path "~/.ghcup/bin")
(add-to-list 'load-path "~/.config/emacs/bin")
(add-to-list 'load-path "/opt/homebrew/bin")
(setenv "PATH" (concat (string-join load-path ":")
                       ":"
                       (getenv "PATH")))

(setq! jka-compr-shell "fish")

(load! "misc.el")
(load! "kbd.el")


(add-to-list 'default-frame-alist '(undecorated-round . t))


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
      (string-join '("^.stack-work$"
                     "\\.hie$"
                     "^.DS_Store$"
                     "^.git$"
                     "^\\.$"
                     "\\.prof$"
                     "\\.tix$"
                     "^.mypy_cache$"
                     "^.pytest_cache$"
                     "^.coverage$"
                     "^dist-newstyle$")
                   "\\|"))

;; autocomplete
(let ((home (getenv "HOME")))
  (setenv "PATH" (concat
                  home "/.ghcup/bin:"
                  home "/.local/bin:"
                  "/usr/local/bin:"
                  home "/Library/Python/3.9/bin:"
                  (getenv "PATH")))
  (add-to-list 'exec-path (concat home "/Library/Python/3.9/bin"))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path (concat home "/.local/bin"))
  (add-to-list 'exec-path (concat home "/.ghcup/bin")))

;; Spells
(use-package! guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config (setq guess-language-languages '(en ru)
                guess-language-langcodes '((en . ("en_US"
                                                  "English"))
                                           (ru . ("ru-yeyo"
                                                  "Russian")))))

(add-hook 'text-mode-hook #'flyspell-mode)


;; (use-package flyspell-correct-ivy
;;   :bind ("C-M-;" . flyspell-correct-wrapper)
;;   :init (setq flyspell-correct-interface #'flyspell-correct-ivy))


;; Haskell
(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :config (setq ormolu-process-path "fourmolu"
                ormolu-extra-args '("-o" "-XTypeApplications" "-o" "-XImportQualifiedPost")
                ormolu-cabal-default-extensions nil
                )
  :bind (:map haskell-mode-map
              ("<f1>" . hoogle)
              ("C-c r" . ormolu-format-buffer)))

(use-package lsp-haskell
  :ensure t
  :config (setq lsp-haskell-server-path "haskell-language-server-wrapper"
                lsp-lens-enable nil
                lsp-ui-sideline-enable 't
                lsp-ui-sideline-show-code-actions 't
                lsp-haskell-formatting-provider "fourmolu"))

(defun rk-haskell-mode-hook ()
  (push "[/\\\\]gen/" lsp-file-watch-ignored)
  ;; (setq haskell-process-path-ghci "stack exec -- ghci")
  (display-fill-column-indicator-mode 't)
  ;; (enable-paredit-mode)
  )

(add-hook 'haskell-mode-hook 'rk-haskell-mode-hook)



(defun rk-inferior-haskell-mode-hook ()
  (setq compilation-first-column 1)
  (setq compilation-error-regexp-alist (cons `("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)?"
                                               1 2 4 ,@(if (fboundp 'compilation-fake-loc)
                                                           '((6)
                                                             nil)))
                                             (cdr (cdr inferior-haskell-error-regexp-alist)))))


;; (use-package dhall-mode
;;   :ensure t
;;   :mode "\\.dhall\\'")
(setq fill-column 120)


;; Formatting

;; (setq format-all-formatters
;;       '(("Shell" (shfmt "-i" "4" "-ci"))))

(setq-hook! 'web-mode-hook +format-with-lsp nil)

;; Clojure

;; (setq-hook! 'clojure-mode-hook +format-with-lsp nil)

;; (use-package! zprint-format
;;   :demand t
;;   :after zprint-format
;;   :config (progn
;;             (setq zprint-format-arguments '("{:search-config? true}" "-w"))            ))

(defun cider-user-reload ()
  (interactive)
  (if (cider-connected-p)
      (progn
        (message "Save & Reload...")
        (save-some-buffers 1)
        (cider-interactive-eval "(user/reload)"
                                nil
                                nil
                                (cider--nrepl-pr-request-map)))
    (message "Not connected to a Clojure REPL")))

(defun my-clojure-mode-hook ()
  (local-set-key (kbd "C-b") 'cider-user-reload))

(use-package! clojure-mode
  :config (progn
            (setq clojure--prettify-symbols-alist nil)
            (remove-hook 'clojure-mode-hook 'prettify-symbols-mode)
            ;; (add-hook 'clojure-mode-hook 'zprint-format-on-save-mode)
            (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))

;; (zprint-format-on-save-mode t)

;; (setq-hook! 'clojure-mode-hook +format-with 'zprint)

;; (setq-hook! 'typescript-mode-hook +format-all nil)
;; (setq-hook! 'web-mode-hook +format-with 'prettier)

;; (use-package! prettier-js
;;  :ensure t
;;  :after typescript-mode
;;  :config (add-hook 'typescript-mode-hook 'prettier-js-mode))


(setq-hook! 'python-mode-hook +format-with-lsp nil)
(setq-hook! 'python-mode-hook +format-all-mode nil)
(setq-hook! 'python-mode-hook +format-with nil)
                                        ;(add-hook! 'python-mode-hook #'python-black-on-save-mode)


(use-package! python-black
  :demand t
  :after python
  :config (setq python-black-command "pipenv run black"))


(defun rk-markdown-mode-hook ()
  (local-set-key (kbd "s-1") 'markdown-insert-header-atx-1)
  (local-set-key (kbd "s-2") 'markdown-insert-header-atx-2)
  (local-set-key (kbd "s-3") 'markdown-insert-header-atx-3)
  (local-set-key (kbd "s-4") 'markdown-insert-header-atx-4)
  (local-set-key (kbd "s-5") 'markdown-insert-header-atx-5))

(use-package markdown-mode
  :ensure t
  :config (progn
            (setq-hook! 'markdown-mode-hook +format-with :none)
            (setq flycheck-markdown-markdownlint-cli-config ".markdownlint.json")
            (add-hook 'markdown-mode-hook 'rk-markdown-mode-hook)
            (add-hook 'markdown-mode-hook 'delete-trailing-whitespace)))

;; (use-package! ox-leanpub
;;   :after org)

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


(global-so-long-mode 0)

;; org-mode
;; (defun rk-org-mode-hook ()
;;   (auto-fill-mode 0)
;;   (visual-line-mode t)
;;   (require 'ob-python)
;;   (org-babel-do-load-languages 'org-babel-load-languages '((python . t)
;;                                                            (forth . t)
;;                                                            (emacs-lisp . t)))
;;   (local-set-key (kbd "s-0") 'org-tree-to-indirect-buffer)
;;   (local-set-key (kbd "s-9") 'org-publish-all)
;;   (local-set-key (kbd "C-q") 'org-unfill-paragraph))

;; (add-hook 'org-mode-hook 'rk-org-mode-hook)

;; (use-package ox-reveal
;;   :ensure t
;;   :init (progn
;;           (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")))

(defun save-and-cider-eval-buffer ()
  (interactive)
  (save-buffer)
  (cider-eval-buffer))

(use-package cider
  :ensure t
  :config (progn
            (setq cider-stacktrace-default-filters '(project))
            (cider-auto-test-mode 't))
  :bind (("<f12>" . cider-format-buffer)
         ("C-c l l" . save-and-cider-eval-buffer)))

;; (use-package lsp-docker
;;   :ensure t)

(use-package treemacs
  :ensure t
  :config (progn
            (setq treemacs-indentation 1
                  treemacs-collapse-dirs 1
                  treemacs-silent-refresh t)
            (treemacs-follow-mode t)
            (treemacs-hide-gitignored-files-mode t)
            (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(add-to-list 'auto-mode-alist '("\\.hurl\\'" . http-mode))


(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package vertico-posframe
  :init (vertico-posframe-mode 1)
  :config (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center))


(setq initial-frame-alist
      '((width . 160)  
        (height . 60)
        (left . 50)
        (top . 80)))
