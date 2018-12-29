;; -*- emacs-lisp -*-

;; Set packages repo
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Set load path
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-theme")

;; Load config
(require 'basic-cfg)
(require 'ui-cfg)
(require 'prog-cfg)
(require 'org-cfg)
(require 'other-cfg)
(require 'keybind-cfg)
(require 'os-cfg)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-lsp lsp-mode lsp-java lsp-javacomp lsp-python lsp-rust lsp-ui company use-package helm helm-fuzzy-find helm-git helm-ls-git helm-ls-hg helm-ls-svn helm-swoop magit magit-filenotify magit-find-file magit-popup neotree yasnippet-snippets yasnippet toml-mode dockerfile-mode yaml-mode coffee-mode ace-jump-mode cmake-mode deft rust-mode lua-mode markdown-mode multi-term graphviz-dot-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
