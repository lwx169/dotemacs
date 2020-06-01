;; -*- emacs-lisp -*-

;;; copy enhance
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )
     
(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
	(let ((beg (get-point begin-of-thing 1))
		  (end (get-point end-of-thing arg)))
	  (copy-region-as-kill beg end)))
  )
     
(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
			   (progn (comint-next-prompt 25535) (yank))
			 (progn (goto-char (mark)) (yank) )))))
	(if arg
		(if (= arg 1)
     		nil
		  (funcall pasteMe))
	  (funcall pasteMe))
	))
;;; copy word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)  
  ;;(paste-to-mark arg)
  (message "word copied!"))

;;; copy line
(defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
	   (message "line copied!"))

(defun switch-to-ansi-term()
  (interactive)
  (switch-to-buffer "*ansi-term*"))

(defun switch-to-minicom()
  (interactive)
  (switch-to-buffer "*minicom*"))

(defun scratch()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun wiki()
  (interactive)
  (find-file "~/Ubox/Org/Wiki/index.org"))

(defun todo()
  (interactive)
  (find-file "~/Ubox/Org/todo.org"))

(defun setting()
  (interactive)
  (find-file "~/.emacs"))

(defun minicom()
  (interactive)
  (ansi-term "minicom")
  (rename-buffer "*minicom*"))

(defun revert-buffer-no-confirm()
  (interactive)
  (revert-buffer t t))

;;;; ace-jump-mode
(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode)
  ("C-x o" . ace-window))

;;;; orgnote
(require 'orgnote)

;;;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'term-mode-hook
          (lambda() (yas-minor-mode -1)))

;;;; erc
(setq erc-ignore-list nil)
(setq erc-hide-list
      '("JOIN" "PART" "QUIT" "MODE"))

;;;; ccrypt
(require 'ps-ccrypt)

;;;; adjust font size by hydra
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "+")
  ("-" text-scale-decrease "-")
  ("0" (text-scale-set 0) :bind nil :exit t))

;;;; pine
(require 'pine)

(provide 'other-cfg)
