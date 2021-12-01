(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))


(defun print-comparison-result (data)
      (print 
        (length
          (remove-if #'null
            (mapcar #'< (butlast data) (cdr data)))))
)

(defun problem1 (data)
  (print-comparison-result data)
)

(defun problem2 (data)
  (print-comparison-result
    (mapcar #'+ (butlast (butlast data)) (butlast (cdr data)) (cdr (cdr data)))
))

(defun load-data () (get-file "data.txt"))

;; (problem1 (load-data))
(problem2 (load-data))
