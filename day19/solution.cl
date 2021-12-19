(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data)))
        (parse-block lines)))

(defun parse-block (lines)
    (if lines
        (let* ((beacons (list)))
            (loop for i in (alexandria:iota (- (length lines) 1) :start 1) do
                (if (equal "" (nth i lines))
                    (return (append (list beacons) (parse-block (subseq lines (+ i 1)))))
                    (setq beacons (append beacons (list (parse-coord (nth i lines))))))))
        nil))

(defun parse-coord (line)
    (mapcar #'parse-integer (split-sequence:split-sequence #\, line)))

(defun calc-dist (xyz1 xyz2)
    (sqrt
        (apply #'+
            (mapcar
                (lambda (x) (* x x))
                (mapcar #'- xyz1 xyz2)))))

;; --- Parsing input done ---

(defun iota (start stop)
    (alexandria:iota (- stop start) :start start))

(defun calc-dists (window)
    (apply #'append
        (loop for i in (iota 0 (- (length window) 1)) collect
            (loop for j in (iota (+ i 1) (length window)) collect
                (list (calc-dist (nth i window) (nth j window)) (list (nth i window) (nth j window)))))))

(defun dict-append (dict k l)
    (setf (gethash k dict) (append (gethash k dict) l)))

(defun dict-keys (dict)
    (alexandria:hash-table-keys dict))

(defun majority-item (l)
    (second (car 
        (sort
            (mapcar
                (lambda (x) (list (count x l :test 'equal) x))
                l)
            #'>
            :key #'car))))

(defun get-correspodences (ref window)
    (let* ((poss (make-hash-table :test 'equal)))
        (mapcar 
            (lambda (x)
                (let* (  (dist (first x))
                        (wpoints (second x))
                        (rpoints (gethash dist ref)))
                    (if rpoints
                        (let ()
                            (dict-append poss (first wpoints) rpoints)
                            (dict-append poss (second wpoints) rpoints))
                        nil)))
            (calc-dists window))
        (if (> (length (dict-keys poss)) 2)
            (mapcar
                (lambda (key) (list key (majority-item (gethash key poss))))
                (dict-keys poss)
            )
            nil)))

(defun get-vector (A B)
    (mapcar #'- B A))

(defun transform-corr (correspodences)
    (let ((transform (apply #'get-vector (first correspodences))))
        (loop for corr in correspodences do
            (print (list transform (apply #'get-vector corr))))))

(let*
    (
        (data (load-data))
        (*ref* (make-hash-table :test 'equal))
    )
    (mapcar
        (lambda (x) (setf (gethash (first x) *ref*) (second x)))
        (calc-dists (car data)))
    (transform-corr (get-correspodences *ref* (nth 2 data)))
    ;; (loop for window in data do
    ;;     ;; (print window)))
        ;; (print (calc-dists window))))

)

;; (print (majority-item (list 1 23 3 "test" "test" "test" 1 5 2 0 0 0 "test" "test" "test" "test" "Test" 0 1 1 1)))