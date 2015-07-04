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

;; (defun copy-paragraph (&optional arg)
;;   "Copy paragraphes at point"
;;   (interactive "P")
;;   (copy-thing 'backward-paragraph 'forward-paragraph arg)
;;   (paste-to-mark arg)
;;   )

;; (global-set-key (kbd "C-c p")         (quote copy-paragraph))

;; (defun beginning-of-string(&optional arg)
;;   "  "
;;   (re-search-backward "[ \t]" (line-beginning-position) 3 1)
;;   (if (looking-at "[\t ]")  (goto-char (+ (point) 1)) )
;;   )
;; (defun end-of-string(&optional arg)
;;   " "
;;   (re-search-forward "[ \t]" (line-end-position) 3 arg)
;;   (if (looking-back "[\t ]") (goto-char (- (point) 1)) )
;;   )

;; (defun thing-copy-string-to-mark(&optional arg)
;;   " Try to copy a string and paste it to the mark
;;      When used in shell-mode, it will paste string on shell prompt by default "
;;   (interactive "P")
;;   (copy-thing 'beginning-of-string 'end-of-string arg)
;;   (paste-to-mark arg)
;;   )

;; (global-set-key (kbd "C-c s") (quote thing-copy-string-to-mark))

;; (defun beginning-of-parenthesis(&optional arg)
;;   "  "
;;   (re-search-backward "[[<(?\"]" (line-beginning-position) 3 1)
;;   (if (looking-at "[[<(?\"]")  (goto-char (+ (point) 1)) )
;;   )
;; (defun end-of-parenthesis(&optional arg)
;;   " "
;;   (re-search-forward "[]>)?\"]" (line-end-position) 3 arg)
;;   (if (looking-back "[]>)?\"]") (goto-char (- (point) 1)) )
;;   )

;; (defun thing-copy-parenxthesis-to-mark(&optional arg)
;;   " Try to copy a parenthesis and paste it to the mark
;;      When used in shell-mode, it will paste parenthesis on shell prompt by default "
;;   (interactive "P")
;;   (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
;;   (paste-to-mark arg)
;;   )
;; (global-set-key (kbd "C-c a")

(defun switch-to-ansi-term()
  (interactive)
  (switch-to-buffer "*ansi-term*"))

(defun switch-to-minicom()
  (interactive)
  (switch-to-buffer "*minicom*"))

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
(require 'ace-jump-mode)

;;;; ace-window
(require 'ace-window)

;;;; orgnote
(require 'orgnote)

;;;; deft
(require 'deft)
(setq deft-text-mode 'org-mode)
(setq deft-extension "org")
(setq deft-directory "~/Org/Wiki")
(setq deft-recursive t)

;;;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;; undo-tree
(require 'undo-tree)


;;;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(defun my-multi-term-dedicated-toggle()
  (interactive)
  (if (equal "*MULTI-TERM-DEDICATED*" (buffer-name))
	(multi-term-dedicated-toggle)
	(progn
	  (multi-term-dedicated-toggle)
	  (multi-term-dedicated-select)
	  )
	)
  )

(equal "other-cfg." (buffer-name))
(buffer-name)

(provide 'other-cfg)
