(require 'flyspell)

(add-hook 'text-mode-hook 'flyspell-mode)
(setq flyspell-delay 1)
(setq flyspell-always-use-popup t)

(defun spell-russian ()
        (interactive)
        (ispell-change-dictionary "russian"))

(defun spell-english ()
        (interactive)
        (ispell-change-dictionary "american"))

(global-set-key "\C-cfse" 'spell-english)
(global-set-key "\C-cfsr" 'spell-russian)
