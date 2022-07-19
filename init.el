;; -*- emacs-lisp -*-

;;; Code:
;;; Set packages repo
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)

;;; Set load path
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/pine")
(add-to-list 'load-path "~/.emacs.d/plugins/lsp-bridge")

;;; Load config
(require 'basic-cfg)
(require 'ui-cfg)
(require 'prog-cfg)
(require 'eshell-cfg)
(require 'org-cfg)
(require 'mail-cfg)
(require 'other-cfg)
(require 'keybind-cfg)
(require 'os-cfg)

;;; Load local config
(cond
  ((file-exists-p "~/.emacs.d/lisp/local-cfg.el")
      (progn
        (message "Load local config ...")
        (require 'local-cfg))))

;;; Set selected package
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gruvbox-theme elvish-mode elfeed-org elfeed neotree flycheck-pos-tip rainbow-delimiters flycheck-rust doom-modeline doom-themes ruby-end meson-mode robot-mode yaml-mode inf-ruby ag haxe-mode zig-mode json-mode nix-mode hl-todo all-the-icons-ivy-rich ivy-rich dracula-theme paredit ac-geiser geiser geiser-chez vala-snippets vala-mode go-mode bison-mode htmlize io-mode web-mode counsel-notmuch notmuch fill-column-indicator rust-mode python-mode flycheck keyfreq gradle-mode lsp-javascript-typescript lsp-python use-package magit-filenotify magit-find-file magit-popup yasnippet yasnippet-snippets toml-mode dockerfile-mode lua-mode cmake-mode plantuml-mode ace-jump-mode calfw calfw-org treemacs-magit groovy-mode protobuf-mode ivy counsel swiper counsel-projectile emacsql emacsql-sqlite)))

;;; init.el ends here
