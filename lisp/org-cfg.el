;; -*- emacs-lisp -*-

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 
		  (lambda () (setq truncate-lines nil)))
(setq org-agenda-span 'day)
(setq org-startup-with-inline-images t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "TASK(a)" "DING(i!)" "PAUS(p!)" "BUG!(b!)" "|" "DONE(d!)" "CANC(c!)" "FIX!(f!)")))

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

;;; enable syntax hightlight in org-mode
(setq org-src-fontify-natively t)

;;; org-draft
(setq org-draft-path "~/.emacs.d/.draft.org")
(setq org-draft-sync-path "~/Org/draft.org")
(defun org-draft()
  (interactive)
  (find-file org-draft-path))
(defun org-draft-sync()
  (interactive)
  (message "syncing draft files ...")
  (if (file-exists-p org-draft-sync-path)
      (delete-file org-draft-sync-path))
  (copy-file org-draft-path org-draft-sync-path))

;;; calfw
(require 'calfw)
(require 'calfw-org)

;;; plantuml
(require 'plantuml-mode)
(setq plantuml-jar-path "~/.emacs.d/plugins/plantuml/plantuml.jar")
(setq org-plantuml-jar-path "~/.emacs.d/plugins/plantuml/plantuml.jar")
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages
                             '((plantuml . t)))

(provide 'org-cfg)
