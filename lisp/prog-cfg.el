;; -*- emacs-lisp -*-

;;; auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (global-auto-complete-mode 1)
;; (setq ac-sources '(ac-source-files-in-current-dir
;;                    ac-source-filename
;;                    ac-source-abbrev
;;                    ac-source-words-in-buffer
;; 				   ac-source-words-in-all-buffer
;;                    ac-source-imenu))

;;; highlight symbo
(require 'auto-highlight-symbol-config)

;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;; template
(require 'template)
(template-initialize)
(setq template-default-directories (cons "~/.emacs.d/templates/" template-default-directories))

;;; magit
(require 'magit)

;;; show which func
(which-func-mode t)

;;; cscope
(add-hook 'c-mode-common-hook
		  '(lambda ()
			 (require 'xcscope)))
(setq cscope-do-not-update-database t)
(setq cscope-edit-single-match nil)

;;; jump between paren
(defun my-match-paren (arg)
  "Press % to jump to matching paren -- lgfang"
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp))
        (t (self-insert-command (or arg 1)))))

;;;;; C/C++

;;; style
(setq c-default-style "linux")
(setq c-basic-offset 4)

;;; Highlight #if 0 to #endif  
(defun c-if-0-font-lock (limit)  
  (save-restriction  
	(widen)  
	(save-excursion  
	  (goto-char (point-min))  
	  (let ((depth 0) str start start-depth)  
		(while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)  
		  (setq str (match-string 1))  
		  (if (string= str "if")  
			  (progn  
				(setq depth (1+ depth))  
				(when (and (null start) (looking-at "\\s-+0"))  
				  (setq start (match-end 0)  
						start-depth depth)))  
			(when (and start (= depth start-depth))  
			  (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)  
			  (setq start nil))  
			(when (string= str "endif")
			  (setq depth (1- depth)))))  
		(when (and start (> depth 0))  
		  (c-put-font-lock-face start (point) 'font-lock-comment-face)))))  
  nil)
  
(defun c-if-0-hook ()  
  (font-lock-add-keywords  
   nil  
   '((c-if-0-font-lock (0 font-lock-comment-face prepend))) 'add-to-end))
  
(add-hook 'c-mode-common-hook 'c-if-0-hook)
(add-hook 'c-mode-hook 'remove-dos-eol)

;;;;; lsp
(require 'lsp-mode)
(use-package lsp-mode
  :config
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp))

(use-package lsp-mode
  :config
  ;;; Enable lsp in all programming modes
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp)

  ;;; Additional lsp-related packages
  (use-package company-lsp)
  (use-package lsp-ui))

;;;;; elisp
(add-to-list 'magic-mode-alist '("-*- emacs-lisp -*-" . emacs-lisp-mode))

;;;;; dot
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;; lua
(require 'lua-mode)
(setq lua-indent-level 4)

;;;; python
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)

;;;; rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;; java
(require 'lsp-javacomp)
(add-hook 'java-mode-hook #'lsp-javacomp-enable)
(use-package lsp-javacomp
  :commands lsp-javacomp-enable
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              ;; Load company-lsp before enabling lsp-javacomp, so that function
              ;; parameter snippet works.
              (require 'company-lsp)
              (lsp-javacomp-enable)
              ;; Use company-lsp as the company completion backend
              (set (make-variable-buffer-local 'company-backends) '(company-lsp))
              ;; Optional company-mode settings
              (set (make-variable-buffer-local 'company-idle-delay) 0.1)
              (set (make-variable-buffer-local 'company-minimum-prefix-length) 1)))
  ;; Optional, make sure JavaComp is installed. See below.
  :config
  (lsp-javacomp-install-server))

;;;; cmake
(require 'cmake-mode)
(setq auto-mode-alist
	  (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
			  auto-mode-alist))


;;;; yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;; toml
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;;;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;; coffee script
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;;;;; company
(add-hook 'after-init-hook 'global-company-mode)

;;;; auto detect indent mode
(defun auto-detect-indent-mode()
  (interactive)
  (let ((sit-tabs-count       0)
        (sit-whitespace-count 0))
    (setq sit-tabs-count
          (count-matches "^	" (point-min) (point-max) nil))
    (setq sit-whitespace-count
          (count-matches "^ [^\*]" (point-min) (point-max) nil))
    (if (>= sit-tabs-count sit-whitespace-count)
        (progn
          (message "tabs(%d) >= whitespaces(%d), indent with tab"
                   sit-tabs-count sit-whitespace-count)
          (setq indent-tabs-mode t))
        (progn
          (message "tabs(%d) < whitespaces(%d), indent with whitespace"
                   sit-tabs-count sit-whitespace-count)
          (setq indent-tabs-mode nil)))
    )
  )

(add-hook 'c-mode-hook 'auto-detect-indent-mode)

(defun switch-indent-mode()
  (interactive)
  (if indent-tabs-mode
      (progn
        (setq indent-tabs-mode nil)
        (message "switch: %s" "indent with whitespace"))
      (progn
        (setq indent-tabs-mode t)
        (message "switch: %s" "indent with tab"))
  )
)

(provide 'prog-cfg)
