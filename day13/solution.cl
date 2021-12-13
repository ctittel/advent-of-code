(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data()
    (let* (  
            (lines (split-sequence:split-sequence 
                        #\Newline 
                        (alexandria:read-file-into-string "input.txt")))
            (coord-lines (subseq lines 0 (position "" lines :test 'equal)))
            (fold-lines (subseq lines (+ (position "" lines :test 'equal) 1) (- (length lines) 1)))
        )
        (list (parse-coords coord-lines) (parse-fold fold-lines))))

(defun parse-coords (data)
    (mapcar (lambda (x) (mapcar #'parse-integer (split-sequence:split-sequence #\, x)))
     data))

(defun parse-fold (data)
    (mapcar (lambda (line) 
            (let* (
                (splitted (split-sequence:split-sequence #\= line))
                (num (parse-integer (nth 1 splitted)))
                (first-part (coerce (car splitted) 'list))
                (dim (car (last first-part)))
            ) (list dim num)))
        data))

(defun fold-scalar (x fold-x)
    (cond   ((< x fold-x) x)
            ((= x fold-x) nil)
            (t (- (* 2 fold-x) x))))

(defun apply-fold (dim foldx coords)
    (let* (
        (rawx (mapcar (lambda (coord) (nth 0 coord)) coords))
        (rawy (mapcar (lambda (coord) (nth 1 coord)) coords))
        (x (if (equal dim #\x)
                (mapcar (lambda (xx) (fold-scalar xx foldx)) rawx)
                rawx))
        (y (if (equal dim #\y)
                (mapcar (lambda (yy) (fold-scalar yy foldx)) rawy)
                rawy)))
    (remove-if (lambda (coord) (or (null (nth 0 coord)) (null (nth 1 coord)))) (mapcar 'list x y))))

(defun problem1 (coords fold)
    (let* ((new-coords (apply-fold (nth 0 fold) (nth 1 fold) coords))
            (hashlist (make-hash-table :test 'equal)))
        (loop for coord in new-coords do 
            (setf (gethash coord hashlist) t))
        (hash-table-count hashlist)))

(defun problem2 (coords folds)
    (let* ((final-coords (reduce (lambda (total x) 
                                    (apply-fold (nth 0 x) (nth 1 x) total)) 
                                    folds :initial-value coords))
            (maxx (apply #'max (mapcar (lambda (x) (car x)) final-coords)))
            (maxy (apply #'max (mapcar (lambda (x) (nth 1 x)) final-coords)))
            (arr (make-array (list (+ 1 maxy) (+ 1 maxx)) :initial-element '_))                        
        )
        (loop for coord in final-coords do (setf (aref arr (nth 1 coord) (nth 0 coord)) 'X))
    arr))

(let* ( (data (load-data))
        (coords (car data))
        (folds (nth 1 data)))
    (print (problem1 coords (car folds)))
    (print (problem2 coords folds))
)
