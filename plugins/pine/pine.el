;; pine

(require 'emacsql)
(require 'emacsql-sqlite)

(defvar pine:resource-path "~/Temp/pine/res")
(defvar pine:cache-path "/Temp/pine/cache")
(defvar pine:db-path "~/Temp/pine/pine.db")
(defvar pine:db (emacsql-sqlite pine:db-path))

(defun get-pine-res-path(target)
  (concat pine-resource-path "/" target))

(setq org-link-abbrev-alist
      '(("pine" . "%(get-pine-res-path)")))


(defun pine:database-initialize()
  (knowledge-tree:create-table)
  (knowledge-tree:insert-node "root" 0)
  (resource:create-table))

(defun knowledge-tree:create-table()
  (emacsql pine:db [:create-table knowledge-tree
                    ([(id integer :primary-key :autoincrement)
                      (name text :not-null)
                      (parent integer :not-null)
                      ])]))

(defun knowledge-tree:insert-node(node-name parent-id)
  (emacsql pine:db [:insert
                    :into knowledge-tree
                    [name, parent]
                    :values ([$s1 $s2])]
           node-name parent-id))

(defun knowledge-tree:query-node(node-id)
  (emacsql pine:db [:select [name, parent]
                    :from knowledge-tree
                    :where (= id $s1)]
           node-id))

(defun knowledge-tree:query-sub-nodes(node-id)
  (emacsql pine:db [:select [id, name]
                    :from knowledge-tree
                    :where (= parent $s1)]
           node-id))

(emacsql pine:db [:select [name, parent]
                  :from knowledge-tree
                  :where (= id 2)])

(defun resource:create-table()
  (message "ok"))

(emacsql pine:db [:drop :table knowledge-tree])
(pine:database-initialize)
(knowledge-tree:insert-node "aaa" 1)
(knowledge-tree:insert-node "bbb" 1)
(knowledge-tree:query-node 1)
(knowledge-tree:query-sub-nodes 3)

;;; add knowledge tree node

;;; add file

;;; add note

;;; add clip (download from a url)

(provide 'pine)
