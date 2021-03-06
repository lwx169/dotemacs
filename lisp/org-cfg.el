;; -*- emacs-lisp -*-

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(setq org-agenda-span 'day)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-start-with-log-mode t)
(setq org-startup-with-inline-images t)
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WORK(k)" "LIFE(f)" "BUY!(b)" "GAME(g)" "LERN(l)" "IDEV(v)" "WRIT(w)" "READ(r!)" "|" "DONE(d!)" "CANC(c!)")))

;;;; set custom commands
(setq org-agenda-custom-commands
      (quote
       (("x" "Select Agenda Mode" org-agenda-select-mode "" nil))))

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
(use-package calfw)
(use-package calfw-org
  :bind
  ([f7] . cfw:open-org-calendar))

;;; plantuml
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/plugins/plantuml.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/plugins/plantuml.jar")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

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

;;; publish
(require 'ox-publish)
;; (require 'ox-rss)
(setq org-publish-project-alist
      '(("blog-posts"
         ;; publish
         :base-directory "~/Org/Blog/posts"
         :base-extension "org"
         :publishing-directory "~/Org/Blog/site"
         :publishing-function org-html-publish-to-html

         ;; format
         :headline-levels 4
         :recursive t
         :with-toc nil
         :with-title t
         :with-date t
         :auto-preamble t

         ;; sitemap
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "hehe"
         :sitemap-style list)

        ("blog-res"
         :base-directory "~/Org/Blog/res/"
         :base-extension ".*"
         :publishing-directory "~/Org/Blog/site/res/"
         :publishing-function org-publish-attachment
         :recursive t)

        ("blog" :components ("blog-posts" "blog-res"))))

;; org-babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)
                               (C . t)
                               (scheme . t)
                               ))

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-folded 'content)
(setq org-hide-block-startup t)

;;; count all subtree todo items
(setq org-hierarchical-todo-statistics nil)

;;; latex export
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"))

(provide 'org-cfg)
