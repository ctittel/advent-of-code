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
    ;; (print (list "dict-append" dict k l))
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
    ;; (print (list "get-correspodences" ref window))
    (let* ((poss (make-hash-table :test 'equal)))
        (mapcar 
            (lambda (x)
                (let* ( (dist (first x))
                        (win-points (second x))
                        (ref-points (gethash dist ref)))
                    (if ref-points
                        (let ()
                            (dict-append poss (first win-points) ref-points)
                            (dict-append poss (second win-points) ref-points))
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

(defun get-transforms-aux (symbols)
    (if symbols
        (apply #'append
            (loop for i in (alexandria:iota (length symbols)) collect
                (mapcar
                    (lambda (x)
                        (append (list (nth i symbols)) x))
                    (get-transforms-aux (append (subseq symbols 0 i) (subseq symbols (+ i 1)))))))
        (list nil)))

(defun get-transforms ()
    (append
        (get-transforms-aux (list '+X '+Y '+Z))
        (get-transforms-aux (list '-X '+Y '+Z))
        (get-transforms-aux (list '+X '-Y '+Z))
        (get-transforms-aux (list '+X '+Y '-Z))
        (get-transforms-aux (list '-X '-Y '+Z))
        (get-transforms-aux (list '-X '+Y '-Z))
        (get-transforms-aux (list '+X '-Y '-Z))
        (get-transforms-aux (list '-X '-Y '-Z))))

(defun x (xyz) (nth 0 xyz))
(defun y (xyz) (nth 1 xyz))
(defun z (xyz) (nth 2 xyz))

(defun apply-transform (transform xyz)
    (mapcar
        (lambda (x)
            (cond
                ((equal x '+X) (x xyz))
                ((equal x '-X) (- (x xyz)))
                ((equal x '+Y) (y xyz))
                ((equal x '-Y) (- (y xyz)))
                ((equal x '+Z) (z xyz))
                ((equal x '-Z) (- (z xyz)))))
        transform))

(defun vector- (A B)
    (mapcar #'- B A))

;; correspodences: list (point in current CRS; point in CRS 0)
;; returns the transform, e.g. (-X, +Z, -Y) + the offset vector
(defun find-transform (correspodences)
    ;; (print (list "correspodences" correspodences))
    (loop for transform in (get-transforms) do
        (let* ((transformed
                (mapcar
                    (lambda (x) (apply-transform transform x))
                    (mapcar #'first correspodences)))
                (offsets
                    (mapcar
                        #'vector-
                        transformed
                        (mapcar #'second correspodences))))
            ;; (print (list "transformed" transformed "offsets" offsets))
            (if (and (every
                        (lambda (x) (equal x (first offsets)))
                        offsets)
                    (>= (length correspodences) 3))
                (return (list transform (second offsets)))
                nil))))

(defun build-ref (points)
    (let ((ref (make-hash-table :test 'equal)))
     (mapcar
        (lambda (dist) (setf (gethash (first dist) ref) (second dist)))
        (calc-dists points))
    ref))

(let*
    (
        (data (load-data))
        (known-beacons (car data))
        (known-transforms (list (list 'X 'Y 'Z) (list 0 0 0)))
        (*ref* (build-ref known-beacons)) ; contains distances and corresponding points
    )

    (setq data (cdr data))

    ;; (mapcar
    ;;     (lambda (x) (setf (gethash (first x) *ref*) (second x)))
    ;;     (calc-dists (car data)))

    ;; (print (get-correspodences *ref* (nth 2 data)))
    (print (find-transform (get-correspodences *ref* (nth 2 data))))
    ;; (loop for window in data do
    ;;     ;; (print window)))
        ;; (print (calc-dists window))))

)

;; (print (majority-item (list 1 23 3 "test" "test" "test" 1 5 2 0 0 0 "test" "test" "test" "test" "Test" 0 1 1 1)))

;; (print (get-transforms))

;; (print (apply-transform (list '-Z '-X '+Y) (list 10 20 30)))
