(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))


(defun get-comparison-result (data)
      (length
        (remove-if #'null
          (mapcar #'< (butlast data) (cdr data))))
)

(defun problem2 (data)
  (get-comparison-result
    (mapcar #'+ (butlast (butlast data)) (butlast (cdr data)) (cdr (cdr data)))
))

(defun load-data () (get-file "data.txt"))

(print (list "Problem 1 solution:" (get-comparison-result (load-data))))
(print (list "Problem 2 solution:" (problem2 (load-data))))
