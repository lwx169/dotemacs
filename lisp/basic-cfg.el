;; -*- emacs-lisp -*-

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-file nil)
(show-paren-mode t)
(setq-default fill-column 100)
(setq ring-bell-function 'ignore)
(desktop-save-mode t)

;;; tab width set
(setq-default tab-width 4)
(setq-default default-tab-width 4)
(setq-default indent-tabs-mode nil)

;;;; use-package
(require 'use-package)
(setq use-package-verbose t)

;;;; electric
(require 'electric)
(electric-indent-mode t)
(electric-pair-mode t)

;;; ido-mode
;; (ido-mode t)

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("\C-xb" . counsel-switch-buffer)
  ("\C-cm" . counsel-imenu)
  ("\C-cl" . counsel-locate)
  ("\C-cs" . counsel-ag))

(use-package swiper
  :bind
  ("\C-s" . swiper))

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :bind
  ("\C-xp" . counsel-projectile-find-file)
  ("\C-cps" . counsel-projectile-switch-project)
  ("\C-cpp" . counsel-projectile-git-grep))

(provide 'basic-cfg)
