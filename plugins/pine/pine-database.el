;;; pine-database

(require 'pine)
(require 'pine-utils)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar pine:db nil)

(defun open-database()
  (setq pine:db (emacsql-sqlite pine:db-path)))

(defun close-database()
  (emacsql-close pine:db)
  (setq pine:db nil))

(defun pine:database-initialize()
  (open-database)
  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'knowledge_tree))])
    (progn
      (knowledge-tree:create-table)
      (emacsql pine:db [:insert
                        :into knowledge-tree
                        [name parent parent]
                        :values (["root" 0 "/"])])))
  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'library))])
    (library:create-table))
  (unless (emacsql pine:db [:select name
                            :from sqlite_master
                            :where (and (= type 'table)
                                        (= name 'note))])
    (note:create-table))
  (close-database))

(defun library:create-table()
  (emacsql pine:db [:create-table library
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (path text :not-null)
                      (size integer :not-null)
                      (category text)
                      (filetype text)
                      (tags text)
                      (meta text)])]))

(defun library:add(item.name item.path item.size item.category item.filetype item.tags)
  (open-database)
  (emacsql pine:db [:insert
                    :into library
                    [name path size category filetype tags]
                    :values ([$s1 $s2 $s3 $s4 $s5 $s6])]
           item.name item.path item.size item.category item.filetype item.tags)
  (close-database))

(defun library:edit(item.id item.name item.category item.filetype item.tags)
  (open-database)
  (emacsql pine:db [:update library
                    :set [(= name $s1) (= category $s2) (= filetype $s3) (= tags $s4)]
                    :where (= id $s5)]
           item.name item.category item.filetype item.tags item.id)
  (close-database))

(defun library:delete(item.id)
  (open-database)
  (emacsql pine:db [:delete
                    :from library
                    :where (= id $s1)]
           item.id)
  (close-database))

(defun library:query-all()
  (open-database)
  (let (query value)
    (setq query (emacsql pine:db [:select [id name category filetype size tags path]
                                  :from library]))
    (dolist (element query value)
      (setcar (nthcdr 4 element) (size2str (nth 4 element)))
      (push (list (car element) (vconcat [] (cdr element))) tabulated-list-entries)))
  (close-database))

(defun library:query-by-word()
  (open-database)
  (let (word query value)
    (setq word (concat "%" pine:query-word "%"))
    (setq query (emacsql pine:db [:select [id name category filetype size tags path]
                                  :from library
                                  :where (or (like name $s1 )
                                             (like category $s1)
                                             (like filetype $s1)
                                             (like tags $s1))]
                         word))
    (dolist (element query value)
      (setcar (nthcdr 4 element) (size2str (nth 4 element)))
      (push (list (car element) (vconcat [] (cdr element))) tabulated-list-entries)))
  (close-database))

(defun note:create-table()
  (emacsql pine:db [:create-table note
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (path text :not-null)])]))

(defun knowledge-tree:create-table()
  (emacsql pine:db [:create-table knowledge-tree
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (parent integer :not-null)
                      (path text :uniq :not-null)])]))

(defun knowledge-tree:insert-node(node-name parent-id full-path)
  (open-database)
  (emacsql pine:db [:insert
                    :into knowledge-tree
                    [name parent path]
                    :values ([$s1 $s2 $s3])]
           node-name parent-id full-path)
  (close-database))

(defun knowledge-tree:query-node(node-id)
  (let (result)
    (open-database)
    (setq result (emacsql pine:db [:select [name parent path]
                                   :from knowledge-tree
                                   :where (= id $s1)]
                          node-id))
    (close-database)
    result))

(defun knowledge-tree:query-sub-nodes(node-id)
  (let (result)
    (open-database)
    (setq result (emacsql pine:db [:select [id name path]
                                   :from knowledge-tree
                                   :where (= parent $s1)]
                          node-id))
    (close-database)
    result))

(provide 'pine-database)

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
