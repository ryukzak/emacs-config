;;; ~/.doom.d/misc.el -*- lexical-binding: t; -*-


(defun org-unfill-paragraph (&optional region)
  "Unfill the region, joining text paragraphs into a single
    logical line. This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (org-fill-paragraph nil region)))
