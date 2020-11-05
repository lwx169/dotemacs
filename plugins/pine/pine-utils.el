;;; pine-utils.el ---

(setq storage-unit (vector "B" "KB" "MB" "GB" "TB"))

(defun format-size (size level)
  (format "%.1f%s" size (elt storage-unit level)))

(defun size2str-iter (size level)
  (if (or (>= level 4) (< size 1024))
      (format-size size level)
    (size2str-iter (/ size 1024.0) (+ level 1))))

(defun size2str(size)
  (size2str-iter size 0))

(provide 'pine-utils)
