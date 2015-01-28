(defun org-open-link-in-one-buffer()
  (interactive)
  (org-open-at-point)
  (delete-other-windows))

(add-hook 'org-mode-hook
		  '(lambda()
			 (define-key org-mode-map (kbd "C-c C-o")   'org-open-link-in-one-buffer)))

(defun orgnote-open()
  (sr-speedbar-open)
  (find-file "~/Org/index.org")
  (delete-other-windows)
  (beginning-of-buffer))

(defun orgnote-close()
  (sr-speedbar-close)
  (kill-buffer "*SPEEDBAR*")
  (switch-to-buffer "*scratch*"))

(defun orgnote-toggle()
  (interactive)
  (if (sr-speedbar-exist-p)
	  (orgnote-close)
	(orgnote-open)))

(define-key global-map [f2] 'orgnote-toggle)

(provide 'orgnote)
