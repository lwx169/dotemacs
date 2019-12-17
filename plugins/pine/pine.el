;; pine

(require 'emacsql)
(require 'emacsql-sqlite)

(defconst pine:edit-buffer "*pine edit*")

(defvar pine:root-path "~/Temp/Pine")
(defvar pine:library-path (concat pine:root-path "/library"))
(defvar pine:resource-path (concat pine:root-path "/resource"))
(defvar pine:db-path (concat pine:root-path "/pine.db"))
(defvar pine:db nil)

(defun get-pine-resource-path(target)
  (concat pine:resource-path "/" target))

(setq org-link-abbrev-alist
      '(("pine" . "%(get-pine-resource-path)")))

(defun pine:initialize()
  ;; TODO: handle more exceptions
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
      (knowledge-tree:insert-node "root" 0)))

  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'library))])
    (library:create-table)))

(defun pine:select-file()
  (let ((current-directory default-directory)
        (file-list nil)
        (selected nil))
    (while current-directory
      (setq current-directory (expand-file-name current-directory))
      (setq promote (concat "Select file: ["  current-directory "] "))
      (setq file-list (directory-files current-directory))
      (setq selected (concat current-directory "/" (ido-completing-read promote file-list)))
      (if (file-directory-p selected)
          (setq current-directory selected)
        (setq current-directory nil)))
    selected))

(defun knowledge-tree:create-table()
  (emacsql pine:db [:create-table knowledge-tree
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (parent integer :not-null)])]))

(defun knowledge-tree:insert-node(node-name parent-id)
  (emacsql pine:db [:insert
                    :into knowledge-tree
                    [name parent]
                    :values ([$s1 $s2])]
           node-name parent-id))

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
         (hash (md5 name))
         (prefix (substring hash 0 2))
         (folder (concat pine:library-path "/" prefix))
         (dest (concat folder "/" name)))
    (unless (file-directory-p folder)
      (make-directory folder))
    (message "Copy file: %s -> %s" source dest)
    (copy-file source dest)
    (concat prefix "/" name)))

(defun library:create-table()
  (emacsql pine:db [:create-table library
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (path text :not-null)
                      (attach text)
                      (type text)
                      (tags text)
                      (meta text)])]))

(defun library:add(item.name item.path item.type item.tags)
  (emacsql pine:db [:insert
                    :into library
                    [name path type tags]
                    :values ([$s1 $s2 $s3 $s4])]
           item.name item.path item.type item.tags))

(defun library:add-by-text(data create)
  (let (info-alist splited key value)
    (setq info-alist (make-list 0 nil))

    ;; parse text
    (dolist (line (split-string data "\n"))
      (if (string-match-p "^[^#].+?\\:.+?" line)
          (progn
            (setq line (string-trim line))
            (setq splited (split-string line "\\:"))
            (setq key (string-trim (car splited)))
            (setq value (string-trim (car (cdr splited))))
            (map-put info-alist (intern key) value))))

    ;; copy file
    (if create
        (let* ((from (map-elt info-alist 'from))
               (to (library:import-item from)))
          (map-put info-alist 'path to)))

    ;; update database
    (library:add (map-elt info-alist 'name)
                 (map-elt info-alist 'path)
                 (map-elt info-alist 'type)
                 (map-elt info-alist 'tags))))

;;; add knowledge tree node

;;; add file
(defun pine-add-file()
  (interactive)
  (let ((file (pine:select-file)))
    (let* ((pine-buffer (get-buffer-create pine:edit-buffer))
           (pine-window (display-buffer-below-selected pine-buffer '((window-height . 20)))))
      (select-window pine-window)
      (kill-region (point-min) (point-max))
      (insert "# Commit with C-c C-c, or abort with C-c C-k.\n")
      (insert "\n")
      (insert (concat "from: " file "\n"))
      (insert (concat "name: " (file-name-base file) "\n"))
      (insert "type: \n")
      (insert "tags: \n")
      (goto-line 4)
      (move-to-column 6)
      (local-set-key (kbd "C-c C-c") 'pine-commit-add-item)
      (local-set-key (kbd "C-c C-k") 'pine-abort-add-item)
      )))

(defun pine-commit-add-item()
  (interactive)
  (if (string= pine:edit-buffer (buffer-name))
      (progn
        (library:add-by-text (buffer-substring-no-properties (point-min) (point-max)) t)
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

(provide 'pine)

;;        _                                             _
;;  _ __ | | __ _ _   _  __ _ _ __ ___  _   _ _ __   __| |
;; | '_ \| |/ _` | | | |/ _` | '__/ _ \| | | | '_ \ / _` |
;; | |_) | | (_| | |_| | (_| | | | (_) | |_| | | | | (_| |
;; | .__/|_|\__,_|\__, |\__, |_|  \___/ \__,_|_| |_|\__,_|
;; |_|            |___/ |___/

(pine:initialize)
(pine-add-file)

(emacsql pine:db [:drop :table knowledge-tree])
(emacsql pine:db [:drop :table library])
(emacsql-close pine:db)
(knowledge-tree:insert-node "aaa" 1)
(knowledge-tree:insert-node "bbb" 1)
(knowledge-tree:query-node 1)
(knowledge-tree:query-sub-nodes 1)

(emacsql pine:db [:select *
                  :from library
                  :where (like name "a%")])