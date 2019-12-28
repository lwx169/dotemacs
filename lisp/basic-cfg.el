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

;;; helm
;; (require 'helm-config)
;; (require 'helm-ls-git)
;; (require 'helm-swoop)
;; (use-package helm
;;   :config
;;   (setq helm-boring-buffer-regexp-list '("\\` " "\\*.+?\\*" "magit.*"))
;;   :bind
;;   ("\C-cm" . helm-jump-use-imenu)
;;   ("\C-co" . helm-jump-use-swoop)
;;   ("\C-cb" . helm-jump-back)
;;   ("\C-xb" . helm-mini)
;;   ("\C-xp" . helm-browse-project)
;;   ("\C-xg" . helm-grep-do-git-grep))

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
  ("\C-xb" . counsel-switch-buffer))

(use-package swiper
  :bind
  ("\C-s" . swiper))

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; eshell 
(setq eshell-save-history-on-exit t 
	  eshell-history-size 2000 
	  eshell-hist-ignoredups t
	  eshell-cmpl-ignore-case t
	  eshell-cp-interactive-query t
	  eshell-ln-interactive-query t
	  eshell-mv-interactive-query t
	  eshell-rm-interactive-query t 
	  eshell-mv-overwrite-files nil 
      eshell-highlight-prompt   t
      eshell-prompt-regexp      "^[^#$\n]* [#>]+ "
      eshell-prompt-function    (lambda nil
                                  (concat
                                   (abbreviate-file-name
                                    (eshell/pwd))
                                   (if
                                       (=
                                        (user-uid)
                                        0)
                                       " # " " >>> "))))

(setq eshell-last-dir-ring-size 500)

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

;; (use-package helm-projectile
;;   :bind
;;   ("\C-cpf" . helm-projectile-find-file)
;;   ("\C-cpr" . helm-projectile-recentf)
;;   ("\C-cps" . helm-projectile-switch-project)
;;   ("\C-cpg" . helm-projectile-grep))

(provide 'basic-cfg)
