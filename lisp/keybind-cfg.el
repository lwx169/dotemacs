;; -*- emacs-lisp -*-

;;; edit
(define-key global-map "\C-x\C-n" 'template-new-file)
(define-key global-map "\M-l" 'goto-line)
(define-key global-map "\C-cw" 'copy-word)
(define-key global-map "\C-cl" 'copy-line)
(define-key global-map "\C-r" 'replace-regexp)
  
;;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;;; program
(define-key global-map [C-up] 'c-beginning-of-defun)
(define-key global-map [C-down] 'c-end-of-defun)
(define-key global-map [f1] 'man-follow)

;;; org-draft
(define-key global-map [f8] 'org-draft)

(define-key global-map [f9] 'notmuch)

;;; workspace switch
(define-key global-map [f10] 'revert-buffer-no-confirm)

;;; ui modify
(define-key global-map [f11] 'toggle-frame-fullscreen)

;;; eshell
(define-key global-map [f12] 'eshell)

;;; window move
(define-key global-map [M-left] 'windmove-left)
(define-key global-map [M-right] 'windmove-right)
(define-key global-map [M-up] 'windmove-up)
(define-key global-map [M-down] 'windmove-down)

;;; match paren
(define-key global-map "$" 'my-match-paren)

(provide 'keybind-cfg)
