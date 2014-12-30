;; -*- emacs-lisp -*-

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 
		  (lambda () (setq truncate-lines nil)))
(setq org-agenda-span 'day)
(setq org-startup-with-inline-images t)
(setq org-todo-keywords
      '((sequence "TDO(t)" "ING(i!)" "BUG(b)" "PLA" "|" "DNE(d!)" "PAU(p!)" "CNL(c!)" "FIX(f!)" "NIL(n!)")))

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

;;; set note-capture
(setq org-default-notes-file "~/Ubox/Org/note.org")
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Ubox/Org/note.org" "Note")
             "* %^{TITLE} %u %^g\n  %? ")
		))

;;; set org files
(setq org-agenda-files '("~/Ubox/Org/note.org" "~/Ubox/Org/todo.org"))

;;; enable syntax hightlight in org-mode
(setq org-src-fontify-natively t)

(provide 'org-cfg)
