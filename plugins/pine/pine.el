;; pine

(require 'emacsql)
(require 'emacsql-sqlite)

(defconst pine:edit-buffer "*Pine Edit*")
(defconst pine:library-list-buffer "*Pine Library List*")
(defconst pine:library-query-buffer "*Pine Library Query*")

;; TODO: use defcustom
(defvar pine:root-path "~/Pine")
(defvar pine:library-path (concat pine:root-path "/library"))
(defvar pine:resource-path (concat pine:root-path "/resource"))
(defvar pine:trash-path (concat pine:root-path "/trash"))
(defvar pine:db-path (concat pine:root-path "/pine.db"))
(defvar pine:db nil)
(defvar pine:query-word nil)
(defvar pine:edit-item-id nil)

(defun get-pine-resource-path(target)
  (concat pine:resource-path "/" target))

(setq org-link-abbrev-alist
      '(("pine" . "%(get-pine-resource-path)")))

(defun pine:initialize()
  (unless (file-directory-p pine:root-path)
    (make-directory pine:root-path))
  (unless (file-directory-p pine:library-path)
    (make-directory pine:library-path))
  (pine:database-initialize))

(defun pine:database-initialize()
  (setq pine:db (emacsql-sqlite pine:db-path))

  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'knowledge_tree))])
    (progn
      (knowledge-tree:create-table)
      (knowledge-tree:insert-node "root" 0 "/")))

  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'library))])
    (library:create-table)))

(defun knowledge-tree:create-table()
  (emacsql pine:db [:create-table knowledge-tree
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (parent integer :not-null)
                      (path text :uniq :not-null)])]))

(defun knowledge-tree:insert-node(node-name parent-id full-path)
  (emacsql pine:db [:insert
                    :into knowledge-tree
                    [name parent path]
                    :values ([$s1 $s2 $s3])]
           node-name parent-id full-path))

(defun knowledge-tree:query-node(node-id)
  (emacsql pine:db [:select [name parent]
                    :from knowledge-tree
                    :where (= id $s1)]
           node-id))

(defun knowledge-tree:query-sub-nodes(node-id)
  (emacsql pine:db [:select [id name]
                    :from knowledge-tree
                    :where (= parent $s1)]
           node-id))

(defun library:import-item(source)
  (let* ((name (file-name-nondirectory source))
         (uuid (uuid-create))
         (prefix (substring uuid 0 2))
         (suffix (file-name-extension source))
         (folder (concat pine:library-path "/" prefix))
         (dest (concat folder "/" uuid "." suffix)))
    (unless (file-directory-p folder)
      (make-directory folder))
    (message "Copy file: %s -> %s" source dest)
    (copy-file source dest)
    (concat prefix "/" uuid "." suffix)))

(defun library:create-table()
  (emacsql pine:db [:create-table library
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (path text :not-null)
                      (category text)
                      (filetype text)
                      (tags text)
                      (meta text)])]))

(defun library:add(item.name item.path item.category item.filetype item.tags)
  (emacsql pine:db [:insert
                    :into library
                    [name path category filetype tags]
                    :values ([$s1 $s2 $s3 $s4 $s5])]
           item.name item.path item.category item.filetype item.tags))

(defun library:edit(item.id item.name item.category item.filetype item.tags)
  (emacsql pine:db [:update library
                    :set [(= name $s1) (= category $s2) (= filetype $s3) (= tags $s4)]
                    :where (= id $s5)]
           item.name item.category item.filetype item.tags item.id))

(defun library:delete(item.id)
  (emacsql pine:db [:delete
                    :from library
                    :where (= id $s1)]
           item.id))

(defun parse-edit-buffer(data)
  (let (info-alist splited key value)
    (setq info-alist (make-list 0 nil))
    (dolist (line (split-string data "\n"))
      (if (string-match-p "^[^#].+?\\:.+?" line)
          (progn
            (setq line (string-trim line))
            (setq splited (split-string line "\\:"))
            (setq key (string-trim (car splited)))
            (setq value (string-trim (car (cdr splited))))
            (map-put info-alist (intern key) value))))
    info-alist))

(defun library:add-by-text(data)
  (let* ((info-alist (parse-edit-buffer data))
         (from (map-elt info-alist 'from))
         (to (library:import-item from)))
    (map-put info-alist 'path to)
    (library:add (map-elt info-alist 'name)
                 (map-elt info-alist 'path)
                 (map-elt info-alist 'category)
                 (map-elt info-alist 'filetype)
                 (map-elt info-alist 'tags))))

(defun library:edit-by-text(id data)
  (let* ((info-alist (parse-edit-buffer data)))
    (library:edit id
                 (map-elt info-alist 'name)
                 (map-elt info-alist 'category)
                 (map-elt info-alist 'filetype)
                 (map-elt info-alist 'tags))))

;;; add knowledge tree node

;;; add file
(defun pine-add-file(&optional initial-input)
  (interactive)
  (counsel--find-file-1
   "Select file: " initial-input
   'pine-add-file-action
   'pine-add-file))

(defun pine-add-file-action(file)
  (let* ((pine-buffer (get-buffer-create pine:edit-buffer))
         (pine-window (display-buffer-below-selected pine-buffer '((window-height . 20)))))
    (select-window pine-window)
    (kill-region (point-min) (point-max))
    (insert "# Commit with C-c C-c, or abort with C-c C-k.\n")
    (insert "\n")
    (insert (concat "from: " file "\n"))
    (insert (concat "name: " (file-name-base file) "\n"))
    (insert "category: \n")
    (insert (concat "filetype: " (file-name-extension file) "\n"))
    (insert "tags: \n")
    (goto-line 4)
    (move-to-column 6)
    (local-set-key (kbd "C-c C-c") 'pine-commit-add-item)
    (local-set-key (kbd "C-c C-k") 'pine-abort-add-item)))

;;; copy from: https://nullprogram.com/blog/2010/05/11/
(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun pine-commit-add-item()
  (interactive)
  (if (string= pine:edit-buffer (buffer-name))
      (progn
        (library:add-by-text (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer)
        (delete-window))
    (message "Not a pine buffer")))

(defun pine-abort-add-item()
  (interactive)
  (if (string= pine:edit-buffer (buffer-name))
      (progn
        (kill-buffer)
        (delete-window))
    (message "Not a pine buffer")))

;;; add note

;;; add clip (download from a url)

;;; list/query library
(defun pine:library-query-refresh()
  (setq tabulated-list-entries nil)
  (if pine:query-word (library:query-by-word)
    (library:query-all)))

(defun library:query-all()
  (let (query value)
    (setq query (emacsql pine:db [:select [id name category filetype tags path]
                                  :from library]))
    (dolist (element query value)
      (push (list (car element) (vconcat [] (cdr element))) tabulated-list-entries))))

(defun library:query-by-word()
  (let (word query value)
    (setq word (concat "%" pine:query-word "%"))
    (setq query (emacsql pine:db [:select [id name category filetype tags path]
                                  :from library
                                  :where (or (like name $s1 )
                                             (like category $s1)
                                             (like filetype $s1)
                                             (like tags $s1))]
                         word))
    (dolist (element query value)
      (push (list (car element) (vconcat [] (cdr element))) tabulated-list-entries))))


;; refactor
(defun pine:library-get-entry-path()
  (let (entry path)
    (setq entry (tabulated-list-get-entry))
    (if entry (progn
                (setq path (aref entry 4))
                (setq path (concat pine:library-path "/" path)))
      path)))

(defun pine:open-file-in-emacs()
  (interactive)
  (let ((path (pine:library-get-entry-path)))
    (message (concat "Open file: " path))
    (find-file path)))

(defun pine:open-file-by-external-app()
  (interactive)
  (let ((path (expand-file-name (pine:library-get-entry-path))))
    (message (concat "Open file: " path))
    (cond
     ((string-equal system-type "gnu/linux")
      (let ((process-connection-type nil))
        (start-process "" nil "xdg-open" path)))
     ((string-equal system-type "darwin")
      (shell-command (concat "open " path)))
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "open" path)))))

(defun pine:edit-library-item()
  (interactive)
  (let ((entry (tabulated-list-get-entry))
        (buffer (get-buffer-create pine:edit-buffer)))
    (setq pine:edit-item-id (tabulated-list-get-id))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "# Commit with C-c C-c, or abort with C-c C-k.\n")
    (insert "\n")
    (insert (concat "name: " (aref entry 0) "\n"))
    (insert (concat "category: " (aref entry 1) "\n"))
    (insert (concat "filetype: " (aref entry 2) "\n"))
    (insert (concat "tags: " (aref entry 3) "\n"))
    (goto-line 3)
    (move-to-column 6)
    (local-set-key (kbd "C-c C-c") 'pine-commit-edit-item)
    (local-set-key (kbd "C-c C-k") 'pine-abort-edit-item)
    ))

(defun pine:delete-libray-item()
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (if (y-or-n-p (concat "Trash item: \"" (aref entry 0) "\" ?" ))
        (progn
          (library:delete id)
          (tabulated-list-revert)
          (unless (file-directory-p pine:trash-path)
            (make-directory pine:trash-path))
          (rename-file (concat pine:library-path "/" (aref entry 4))
                       (concat pine:trash-path "/" (file-name-nondirectory (aref entry 4))))
          (message "Deleted"))
      (message "Canceled"))))

(defun pine-commit-edit-item()
  (interactive)
  (if (string= pine:edit-buffer (buffer-name))
      (let ((id pine:edit-item-id))
        (setq pine:edit-item-id -1)
        (library:edit-by-text id (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer)
        (tabulated-list-revert))
    (message "Not a pine buffer")))

(defun pine-abort-edit-item()
  (interactive)
  (if (string= pine:edit-buffer (buffer-name))
      (kill-buffer)
    (message "Not a pine buffer")))

(define-derived-mode pine:library-query-mode tabulated-list-mode "Pine Library"
  "Major mode for listing items in pine library."
  (setq tabulated-list-format [("Name"     32 t)
                               ("Category" 16 t)
                               ("Filetype" 16 t)
                               ("Tags"     16 t)])
  (setq tabulated-list-sort-key (cons "Category" nil))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'pine:library-query-refresh nil t)
  (local-set-key (kbd "RET") 'pine:open-file-in-emacs)
  (local-set-key (kbd "o") 'pine:open-file-by-external-app)
  (local-set-key (kbd "e") 'pine:edit-library-item)
  (local-set-key (kbd "d") 'pine:delete-libray-item))

(defun pine-list-library()
  (interactive)
  (setq pine:query-word nil)
  (pop-to-buffer pine:library-list-buffer)
  (pine:library-query-mode)
  (pine:library-query-refresh)
  (tabulated-list-print))

(defun pine-query-library()
  (interactive)
  (setq pine:query-word (read-string "Query: "))
  (pop-to-buffer pine:library-query-buffer)
  (pine:library-query-mode)
  (pine:library-query-refresh)
  (tabulated-list-print)
  (highlight-regexp pine:query-word)
  (local-set-key (kbd "r") 'pine-query-library))

(pine:initialize)
(provide 'pine)

;;        _                                             _
;;  _ __ | | __ _ _   _  __ _ _ __ ___  _   _ _ __   __| |
;; | '_ \| |/ _` | | | |/ _` | '__/ _ \| | | | '_ \ / _` |
;; | |_) | | (_| | |_| | (_| | | | (_) | |_| | | | | (_| |
;; | .__/|_|\__,_|\__, |\__, |_|  \___/ \__,_|_| |_|\__,_|
;; |_|            |___/ |___/
;;
;; (emacsql pine:db [:drop :table knowledge-tree])
;; (emacsql pine:db [:drop :table library])
;; (emacsql-close pine:db)
;; (knowledge-tree:insert-node "aaa" 1)
;; (knowledge-tree:insert-node "bbb" 1)
;; (knowledge-tree:query-node 1)
;; (knowledge-tree:query-sub-nodes 1)
