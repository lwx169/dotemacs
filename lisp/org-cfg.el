;; -*- emacs-lisp -*-

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 
		  (lambda () (setq truncate-lines nil)))
(setq org-agenda-span 'day)
(setq org-startup-with-inline-images t)
(setq org-todo-keywords
      '((sequence "PROJ(p)" "READ(r)" "SOUR(s)" "TODO(t!)" "|" "FINS(f!)" "DONE(d!)" "PAUS(u!)" "CANC(c!)")))

;;; workflow: PROJ -----> TASK --> TODO --> DONE -----------------------> FINS
;;;                  \--> TASK --> TODO --> CANC --------------------/
;;;                  \--> TASK --> TODO --> PAUS --> TODO --> DONE--/

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

;;; set org files
(setq org-agenda-files '("~/Org/Todo/projects.org" "~/Share/TP-LINK/work.org"))

;;; enable syntax hightlight in org-mode
(setq org-src-fontify-natively t)

;;; calfw
(require 'calfw)
(require 'calfw-org)

(provide 'org-cfg)
