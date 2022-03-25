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

;;; line number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

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
  (add-hook 'c-mode-common-hook #'c-if-0-hook)
  (add-hook 'c-mode-hook #'remove-dos-eol)
  (add-hook 'c-mode-hook #'hs-minor-mode)
  (add-hook 'c-mode-hook #'hide-ifdef-mode))


;;; update citre related things
(defun citre-update-all ()
  (interactive)
  (message "update ctags")
  (citre-update-this-tags-file)
  (message "update gtags")
  (citre-global-update-database))

;;; citre
(use-package citre
  :defer t
  :init
  (require 'citre-config)
  (global-set-key (kbd "M-.") 'citre-jump)
  (global-set-key (kbd "M-,") 'citre-jump-back)
  (global-set-key (kbd "M-\\") 'citre-peek)
  (global-set-key (kbd "C-c u") 'citre-update-all)
  :config
  (setq
   citre-project-root-function #'projectile-project-root
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode))
  :custom
  (citre-peek-file-content-height 16 "set citre peek content height")
  :custom-face
  (citre-peek-border-face ((t (:extend t :background "#262833" :overline t)))))

(use-package citre-global
  :ensure nil
  :defer t
  :init
  (global-set-key (kbd "M-?") 'citre-jump-to-reference)
  (with-eval-after-load 'citre-peek
    (define-key citre-peek-keymap (kbd "M-l r")
      'citre-peek-through-references)))

(require 'gradle-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . gradle-mode))
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;;; lua
(use-package lua-mode
  :mode
  ("\\.lua\\'" . lua-mode)
  :custom
  (lua-indent-level 2 "Set indent level to 2 whitespace"))

;;; perl
(use-package perl-mode
  :mode
  ("\\.pl\\'" . perl-mode)
  ("\\.pm\\'" . perl-mode)
  ("\\.t\\'" . perl-mode)
  :custom
  (perl-indent-level 4 "Set indent level to 4 whitespace"))

;;; ruby
(use-package ruby-mode
  :mode
  ("\\.rb\\'" . ruby-mode))

(use-package inf-ruby
  :hook
  (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-end
  :hook
  (ruby-mode . ruby-end-mode))

;;; python
(use-package python
  :mode
  ("\\.py\\'" . python-mode)
  :config
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nill))

;;; rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'lsp))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; go
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

;;; company
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  :hook
  (prog-mode . company-mode))

;;; auto detect indent mode
(defun auto-detect-indent-mode()
  (interactive)
  (let ((sit-tabs-count       0)
        (sit-whitespace-count 0))
    (setq sit-tabs-count
          (count-matches "^	" (point-min) (point-max) nil))
    (setq sit-whitespace-count
          (count-matches "^ [^\*]" (point-min) (point-max) nil))
    (if (> sit-tabs-count sit-whitespace-count)
        (progn
          (message "tabs(%d) > whitespaces(%d), indent with tab"
                   sit-tabs-count sit-whitespace-count)
          (setq indent-tabs-mode t))
        (progn
          (message "tabs(%d) =< whitespaces(%d), indent with whitespace"
                   sit-tabs-count sit-whitespace-count)
          (setq indent-tabs-mode nil)))))

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
(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)

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
