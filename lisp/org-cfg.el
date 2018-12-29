;; -*- emacs-lisp -*-

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 
		  (lambda () (setq truncate-lines nil)))
(setq org-agenda-span 'day)
(setq org-startup-with-inline-images t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "TASK(a)" "PUZZ(z)" "DING(i!)" "BUG!(b!)" "|" "DONE(d!)" "PAUS(p!)" "CANC(c!)" "FIX!(f!)" "WTF!(w!)" )))

;;; set prioprity level
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?E)
(setq org-default-priority ?C)

;;; set prioprity face
(setq org-priority-faces
  '((?A . (:foreground "red" :weight bold))
    (?B . (:foreground "DarkOrange" :weight bold))
    (?C . (:foreground "yellow" :weight bold))
    (?D . (:foreground "DodgerBlue" :weight bold))
    (?E . (:foreground "SkyBlue" :weight bold))
))

;;; load agenda from Org directory
(setq org-agenda-file-path "~/Org/.hide/agenda.el")
(defun load-org-agenda()
  (interactive)
  (if (file-exists-p org-agenda-file-path)
      (load org-agenda-file-path)
    (message "org agent file not existed!"))
  )
(load-org-agenda)

;;; enable syntax hightlight in org-mode
(setq org-src-fontify-natively t)

;;; org-draft
(setq org-draft-path "~/Org/.hide/draft.org")
(defun org-draft()
  (interactive)
  (find-file org-draft-path))

;;; calfw
(require 'calfw)
(require 'calfw-org)

;;; org screenshot
(defun org-screenshot ()
  (interactive)
  (unless (file-exists-p ".res")
    (make-directory ".res"))
  (setq filename
        (concat
         (make-temp-name
          (concat ".res/"
                  (buffer-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

;;; worklog
(defun worklog()
  (interactive)
  (find-file "~/Org/WorkLog/MuLangCloud/2017.org"))

(provide 'org-cfg)
