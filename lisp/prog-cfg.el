;; -*- emacs-lisp -*-

;;; emacs config
(which-func-mode t)

;;; template
(use-package template
  :config
  (template-initialize)
  (setq template-default-directories (cons "~/.emacs.d/templates/" template-default-directories)))

;;; yasnippet
(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;; magit
(require 'magit)

;;; jump between paren
(defun my-match-paren (arg)
  "Press % to jump to matching paren -- lgfang"
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp))
        (t (self-insert-command (or arg 1)))))

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

;;; linum
(use-package linum-mode
  :hook (prog-mode))

;;; fci-mode
(setq fci-rule-column 120)

;;; lsp
(use-package lsp-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-completion-show-detail nil)
  (setq lsp-completion-show-kind nil)
  (setq lsp-diagnostic-package :none)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  :bind
  ("\C-cd" . lsp-ui-peek-find-definitions)
  ("\C-cr" . lsp-ui-peek-find-references)
  ("\C-ci" . lsp-ui-peek-find-implementation)
  ("\C-cx" . lsp-execute-code-action))

;;; flycheck
(use-package flycheck-mode
  :hook (prog-mode))

;;; c/c++
(use-package cc-mode
  :mode
  ("\\.c\\'" . c-mode)
  ("\\.cc\\'" . c++-mode)
  ("\\.cpp\\'" . c++-mode)
  ("\\.cxx\\'" . c++-mode)
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  :init
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-common-hook #'c-if-0-hook)
  (add-hook 'c-mode-hook #'remove-dos-eol)
  (add-hook 'c-mode-hook #'hs-minor-mode)
  (add-hook 'c-mode-hook #'hide-ifdef-mode))

(use-package ccls
  :config
  (setq ccls-executable "/usr/bin/ccls"))

;;; java
(use-package lsp-java
  :init
  (add-hook 'java-mode-hook #'lsp))

(require 'gradle-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . gradle-mode))
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;;; lua
(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode)
  :config
  (setq lua-indent-level 4))

;;; ruby
(use-package ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode))

(use-package inf-ruby
  :hook
  (ruby-mode . inf-ruby-minor-mode))

;;; python
(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nill))

;;(autoload 'jedi:setup "jedi" nil t)
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)

;;;; rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'lsp))

;;;; go
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook #'lsp))

;;;; io
(use-package io-mode
  :mode "\\.io\\'")

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

;;; dot
(use-package graphviz-dot-mode
  :mode
  ("\\.dot\\'" . graphviz-dot-mode))

;;;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;; coffee script
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

;; protobuf
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;;;; web
(use-package web-mode
  :mode "\\.ftl\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

;;;; lex/yacc
(use-package bison-mode
  :mode
  ("\\.l\\'" . bison-mode)
  ("\\.y\\'" . bison-mode)
  :config
  (setq indent-tabs-mode nil))

;;;; scheme
;; (setq scheme-program-name "chez")
;; (setq geiser-chez-binary "chez")
(setq geiser-active-implementations '(chez))

;;; haxe
(use-package haxe-mode
  :mode
  ("\\.hx\\'" . haxe-mode))

;;;;; company
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60))

;; With use-package:
(use-package company-box
  :config
  (setq company-box-doc-enable nil)
  (setq company-box-scrollbar nil)
  :hook (company-mode . company-box-mode))

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
      ))

;;; xref
(defun xref-kill-window()
  (interactive)
  (let ((window (get-buffer-window "*xref*")))
    (if window (delete-window window)))
  (message "ok"))
(define-key global-map "\M-'" 'xref-kill-window)

;;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; highlight TODO
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

;;; add execute permission for some scripts
(defun save-script-executable ()
  (let ((script-shebang-patterns
         (list "^#!.*perl.*"
               "^#!.*sh"
               "^#!.*bash")))
    (if (not (file-executable-p buffer-file-name))
        (save-excursion
          (goto-char (point-min))
          (dolist (shebang script-shebang-patterns)
            (if (looking-at shebang)
                (set-file-modes buffer-file-name
                                (logior (file-modes buffer-file-name) #o100))))))))

(add-hook 'after-save-hook 'save-script-executable)

(provide 'prog-cfg)
