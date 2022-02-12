(defun org-open-link-in-one-buffer()
  (interactive)
  (org-open-at-point)
  (delete-other-windows))

(add-hook 'org-mode-hook
		  '(lambda()
			 (define-key org-mode-map (kbd "C-c C-o")   'org-open-link-in-one-buffer)))

(provide 'orgnote)
