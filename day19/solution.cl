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

;; --- Parsing input done ---

(defun calc-dist (xyz1 xyz2)
    ;; (sqrt
    (apply #'+
        (mapcar
            (lambda (x) (* x x))
            (mapcar #'- xyz1 xyz2))))


(defun iota (start stop)
    (alexandria:iota (- stop start) :start start))

(defun calc-dists (window)
    (apply #'append
        (loop for i in (iota 0 (- (length window) 1)) collect
            (loop for j in (iota (+ i 1) (length window)) collect
                (list (calc-dist (nth i window) (nth j window)) (list (nth i window) (nth j window)))))))

(defun build-ref (points)
    (let ((ref (make-hash-table :test 'equal)))
     (mapcar
        (lambda (dist)
            (dict-append ref (first dist) (second dist)))
            ;; (setf (gethash (first dist) ref) (second dist)))
        (calc-dists points))
    ref))

(defun dict-append (dict k l)
    (setf (gethash k dict) (append (gethash k dict) l)))

(defun dict-keys (dict)
    (alexandria:hash-table-keys dict))

(defun majority-item (l)
    (loop for item in l do
        (if (>= (count item l) (/ (length l) 2))
            (return item)
            nil)))

    ;; (second (car 
    ;;     (print (sort
    ;;         (mapcar
    ;;             (lambda (x) (list (count x l :test 'equal) x))
    ;;             l)
    ;;         #'>
    ;;         :key #'car)))))

;; returns: list of pairs (CRS X point , CRS 0 point)
(defun get-correspodences (dist->CRS0pts window)
    (let* ( (CRSXpt->CRS0pts (make-hash-table :test 'equal)))
        (loop for (dist CRSXpts) in (calc-dists window ) do
            (loop for CRS0pt in (gethash dist dist->CRS0pts ) do
                (dict-append CRSXpt->CRS0pts (first CRSXpts) (list CRS0pt))
                (dict-append CRSXpt->CRS0pts (second CRSXpts) (list CRS0pt))))
        (if (> (length (alexandria:hash-table-keys CRSXpt->CRS0pts)) 10)
            (loop for Xpt  in (alexandria:hash-table-keys CRSXpt->CRS0pts)
                when (> (length (gethash Xpt CRSXpt->CRS0pts)) 10)
                    collect (list Xpt (majority-item (gethash Xpt CRSXpt->CRS0pts))))
            nil)))

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
    (if (or
            (null transform)
            (null xyz)
            (position 'nil transform)
            (position 'nil xyz))
        (error (list "ERROR IN apply-transform INVALID TRANSFORM" transform xyz)))
    (mapcar
        (lambda (x)
            (cond
                ((equal x '+X) (x xyz))
                ((equal x '-X) (- (x xyz)))
                ((equal x '+Y) (y xyz))
                ((equal x '-Y) (- (y xyz)))
                ((equal x '+Z) (z xyz))
                ((equal x '-Z) (- (z xyz)))
                (t (list "ERROR INVALID" x))))
        transform))

(defun apply-transform-offset-l (transform offset l)
    (mapcar
        (lambda (xyz) (mapcar #'+ (apply-transform transform xyz) offset))
        l))

;; correspodences: list of pairs (point in current CRSX; corresponding point in CRS0)
;; returns the transform, e.g. (-X, +Z, -Y) + the offset vector
(defun find-transform-offset (correspodences)
    (let (  (CRSXpts (mapcar #'first correspodences))
            (CRS0pts (mapcar #'second correspodences)))
        (loop for transform in (get-transforms) do
            (let* ( (offset (mapcar #'- (car CRS0pts) (apply-transform transform (car CRSXpts))))
                    (CRSX0pts (apply-transform-offset-l transform offset CRSXpts)))
                (if (every
                        #'equal
                        CRS0pts
                        CRSX0pts)
                    (return (list transform offset))
                    "DONT RETURN THIS PLEASE")))))

(defun merge-lists (a b)
    (remove-duplicates (append a b) :test 'equal))

;; input: list of known points in CRS 0, unmatched windows in different CRS's
;; output: all points in CRS 0
(defun merge-windows (known-points windows)
    (print (list "merge-windows" "known-points:" (length known-points) "windows:" (length windows)))
    (if windows
        (let ((ref (build-ref known-points)))
            (loop for i in (alexandria:iota (length windows)) do
                (let* ( (current-window (nth i windows))
                        (correspodences (get-correspodences ref current-window)))
                    (if correspodences
                        (let* ( (to (find-transform-offset correspodences))
                                (transform (first to))
                                (offset (second to)))
                            (if transform
                                (return 
                                    (merge-windows
                                        (merge-lists
                                            known-points
                                            (apply-transform-offset-l transform offset current-window))
                                        (append (subseq windows 0 i) (subseq windows (+ i 1)))))
                                nil)))))
            ;; (print (list "LOOP DONE REMAINING WINDOW=" windows))
            )
        known-points))

(let*
    (
        (data (load-data))
        (known-beacons (car data))
        (known-transforms (list (list 'X 'Y 'Z) (list 0 0 0)))
        (*ref* (build-ref known-beacons)) ; contains distances and corresponding points
    )

    (setq data (cdr data))

    (print (merge-windows known-beacons data))

    ;; (mapcar
    ;;     (lambda (x) (setf (gethash (first x) *ref*) (second x)))
    ;;     (calc-dists (car data)))

    ;; (print (get-correspodences *ref* (nth 2 data)))
    ;; (print (find-transform (get-correspodences *ref* (nth 2 data))))
    ;; (loop for window in data do
    ;;     ;; (print window)))
        ;; (print (calc-dists window))))

)

;; A wrong: 311 (too low); 325 (too low)
