(add-to-list 'load-path "~/.emacs.d") 

(setq exec-path (split-string "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/X11/bin:/Users/ryukzak/opt/bin:/Users/ryukzak/opt/scala/bin/:/Users/ryukzak/opt/clojure-contrib/launchers/bash" ":"))
(setenv "PATH" "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/X11/bin:/Users/ryukzak/opt/bin:/Users/ryukzak/opt/scala/bin/:/Users/ryukzak/opt/clojure-contrib/launchers/bash")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-verbose)

(setq el-get-sources '(;; magit psvn org-mode haskell-mode auto-complete paredit
                       ;;       erlware-mode scala-mode distel slime
                       ;;       clojure-mode color-theme ; dired-sync
                       ;;       yasnippet ahg escreen undo-tree
                       ;;       wikipedia-mode mediawiki
                             (:name ergoemacs-keybindings
                                    :type git-svn
                                    :url "http://ergoemacs.googlecode.com/svn/trunk/ergoemacs/ergoemacs-keybindings")
                             (:name dired-sync
                                    :type git
                                    :url "https://github.com/ryukzak/dired-sync.git"
                                    :features dired-sync)
                             ))

(el-get 'sync '(magit psvn org-mode haskell-mode auto-complete paredit
                             erlware-mode scala-mode distel slime
                             clojure-mode color-theme ; dired-sync
                             yasnippet ahg escreen undo-tree
                             wikipedia-mode mediawiki
                             dired-sync                            
                             ))

(push "~/.emacs.d" load-path)
(push "~/.emacs.d/elisp" load-path)

(load "~/.emacs.d/rc-face.el")
(load "~/.emacs.d/rc-control.el")
(load "~/.emacs.d/rc-color-theme.el")

(load "~/.emacs.d/rc-dired.el")

(load "~/.emacs.d/rc-objc.el")
(load "~/.emacs.d/rc-auto-complete.el")

(load "~/.emacs.d/rc-lisp.el")
(load "~/.emacs.d/rc-slime.el")
(load "~/.emacs.d/rc-clojure.el")

(load "~/.emacs.d/rc-parentheses.el")
(load "~/.emacs.d/rc-ruby.el")
(load "~/.emacs.d/rc-workspace.el")
(load "~/.emacs.d/rc-hg.el")
(load "~/.emacs.d/rc-git.el")
(load "~/.emacs.d/rc-ido.el")
(load "~/.emacs.d/rc-ispell.el")
(load "~/.emacs.d/rc-jabber.el")
(load "~/.emacs.d/rc-erlang.el")
(load "~/.emacs.d/rc-yasnippet.el")
(load "~/.emacs.d/rc-org.el")
(load "~/.emacs.d/rc-scala.el")
(load "~/.emacs.d/rc-vhdl.el")
(load "~/.emacs.d/rc-latex.el")
(load "~/.emacs.d/rc-noweb.el")
(load "~/.emacs.d/rc-fold.el")
(load "~/.emacs.d/rc-wiki.el")
(load "~/.emacs.d/rc-org.el")
(load "~/.emacs.d/rc-speedbar.el")
(load "~/.emacs.d/rc-haskell.el")

(setq default-input-method "russian-computer")

(put 'downcase-region 'disabled nil)

;; save session
(add-to-list 'desktop-locals-to-save 'buffer-file-coding-system)
(add-to-list 'desktop-locals-to-save 'tab-width)
(desktop-save-mode 't)
