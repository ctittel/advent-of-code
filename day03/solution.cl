(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (mapcar #'digit-char-p (coerce line 'list)))))

(defun load-data () (load-file "input.txt"))

(defun get-avg (num-lines sum)
    (let ((sum2 (* 2 sum)))
        (cond ((> num-lines sum2) 0)
              (t 1)
        ) 
    ))

(defun reverse-bit (one-bit)
    (cond ((= one-bit 0) 1)
            (t 0)
    ))

(defun binlist-to-dec (list)
    (reduce (lambda (x y) (+ (* 2 x) y)) list))

(defun problem1 (lists)
    (let* ((sums (apply #'mapcar #'+ lists))
            (main (map 'list (lambda (x) (get-avg (length lists) x)) sums))
            (inverse (map 'list (lambda (x) (reverse-bit x)) main))
        )
        (* 
            (binlist-to-dec main)
            (binlist-to-dec inverse))
))

(defun get-sublist (lists list-pos)
    (nth list-pos (apply #'mapcar #'list lists))
)

(defun filter-bit (lists required-bit list-pos)
    (remove-if-not (lambda (sublist) (= required-bit (nth list-pos sublist))) lists)
)

(defun get-dominant-bit (lists list-pos)
    (get-avg (length lists) (apply #'+ (get-sublist lists list-pos)))
)

(defun filter-dominant (lists list-pos selector-fn)
    (if (= (length lists) 1)
        (nth 0 lists)
        (let* ((dominant-bit (funcall selector-fn lists list-pos)))
            (filter-dominant  (filter-bit lists dominant-bit list-pos) (+ 1 list-pos) selector-fn)
        )
    )
)

(defun problem2 (lists)
    (* 
        (binlist-to-dec (filter-dominant lists 0 #'get-dominant-bit))
        (binlist-to-dec (filter-dominant lists 0 (lambda (lists list-pos) (reverse-bit (get-dominant-bit lists list-pos)))))
    )
)

(print (problem1 (load-data)))
(print (problem2 (load-data)))
