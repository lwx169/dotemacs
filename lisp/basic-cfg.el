;; -*- emacs-lisp -*-

(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-file nil)
(show-paren-mode t)
(setq-default fill-column 100)
(setq ring-bell-function 'ignore)
(desktop-save-mode t)

;;; tab width set
(setq tab-width 4)
(setq default-tab-width 4)
(setq indent-tabs-mode)

;;;; electric
(require 'electric)
(electric-indent-mode t)
(electric-pair-mode t)

;;; ido-mode
(ido-mode t)

;;; helm
(require 'helm-config)
(require 'helm-ls-git)

;;; sdcv
(require 'sdcv)

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
                                       " # " " >>> ")))
	  )


;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;;; neotree
(require 'neotree)

;;;; htmlize
(require 'htmlize)

;;;; sr-speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

(provide 'basic-cfg)
