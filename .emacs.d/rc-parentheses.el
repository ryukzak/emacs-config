;; (defmacro parentheses (name open close)
;; 	`(progn
;; 		 (defun ,name()
;; 			 (interactive)
;; 			 (if (and transient-mark-mode mark-active)
;; 					 (let ((b (region-beginning))
;; 								 (e (region-end)))
;; 						 (goto-char (min b e))
;; 						 (insert ,open)
;; 						 (goto-char (1+ (max b e)))
;; 						 (insert ,close))
;; 				 (progn
;; 					 (insert ,open)
;; 					 (insert ,close)
;; 					 (backward-char 1))))
;; 		 (global-set-key ,open (quote ,name))))

;; (parentheses insert-parentheses "(" ")")
;; (parentheses insert-square-parentheses "[" "]")
;; (parentheses insert-braces-parentheses "{" "}")
;; (parentheses insert-quote-parentheses "\"" "\"")

;; (defun backspace ()
;; 	(interactive)
;; 	(setq p (point))
;; 	(goto-char (1- p))
;; 	(if (or (search-forward "()" (1+ p) t 1)
;; 					(search-forward "[]" (1+ p) t 1)
;; 					(search-forward "\"\"" (1+ p) t 1)
;; 					(search-forward "{}" (1+ p) t 1))
;; 			(replace-match "" nil t)
;; 		(progn
;; 			(goto-char p)
;; 			(backward-delete-char-untabify 1))))
;; (global-set-key (kbd "<backspace>") 'backspace)

(require 'paredit)

(define-key paredit-mode-map (kbd "M-v") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-V") 'paredit-forward-barf-sexp)

(define-key paredit-mode-map (kbd "M-w") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-W") 'paredit-backward-barf-sexp)
