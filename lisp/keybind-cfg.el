;; -*- emacs-lisp -*-

;;; helm
(define-key global-map "\C-cm" 'helm-jump-use-imenu)
(define-key global-map "\C-co" 'helm-jump-use-swoop)
(define-key global-map "\C-cb" 'helm-jump-back)
(define-key global-map "\C-xb" 'helm-mini)
(define-key global-map "\C-xp" 'helm-browse-project)
(define-key global-map "\C-xg" 'helm-grep-do-git-grep)

;;; edit
(define-key global-map "\C-x\C-n" 'template-new-file)
(define-key global-map "\M-l" 'goto-line)
(define-key global-map "\C-cw" 'copy-word)
(define-key global-map "\C-cl" 'copy-line)
  
;;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;;; program
(define-key global-map [C-up] 'c-beginning-of-defun)
(define-key global-map [C-down] 'c-end-of-defun)
(define-key global-map [f1] 'man-follow)

;; ;;; neotree
;; (define-key global-map [f2] 'neotree-toggle)

;;; sr-speedbar
;; (define-key global-map [f2] 'sr-speedbar-toggle)

;;; sdcv
(define-key global-map [f5] 'sdcv-search-pointer+)
(define-key global-map [f6] 'sdcv-search-input+)

;;; org-draft
(define-key global-map [f8] 'org-draft)

;;; workspace switch
(define-key global-map [C-f12] 'multi-term-next)
(define-key global-map [f10] 'revert-buffer-no-confirm)

;;; ui modify
(define-key global-map [f11] 'toggle-frame-fullscreen)

;;; window move
(define-key global-map [M-left] 'windmove-left)
(define-key global-map [M-right] 'windmove-right)
(define-key global-map [M-up] 'windmove-up)
(define-key global-map [M-down] 'windmove-down)

;;; match paren
(define-key global-map "$" 'my-match-paren)

;;; ace-jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; ace-window
(define-key global-map (kbd "C-x o") 'ace-window)

(provide 'keybind-cfg)
