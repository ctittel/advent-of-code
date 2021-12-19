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

(defparameter *min-overlap* 12)

(defun calc-dist (xyz1 xyz2)
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
        (calc-dists points))
    ref))

(defun dict-append (dict k l)
    (setf (gethash k dict) (append (gethash k dict) l)))

(defun dict-keys (dict)
    (alexandria:hash-table-keys dict))

(defun majority-item (l)
    (loop for item in l do
        (if (>= (count item l) (ash (length l) -1))
            (return item)
            nil)))

;; returns: list of pairs (CRS X point , CRS 0 point)
(defun get-correspodences (CRS0pts window)
    (let* ( (dist->CRS0pts (build-ref CRS0pts))
            (Xpt->0pts (make-hash-table :test 'equal)))
        (loop for (dist CRSXpts) in (calc-dists window ) do
            (loop for CRS0pt in (gethash dist dist->CRS0pts ) do
                (dict-append Xpt->0pts (first CRSXpts) (list CRS0pt))
                (dict-append Xpt->0pts (second CRSXpts) (list CRS0pt))))
        (if (>= (length (alexandria:hash-table-keys Xpt->0pts)) *min-overlap*)
            (loop for Xpt  in (alexandria:hash-table-keys Xpt->0pts)
                when (and (>= (length (gethash Xpt Xpt->0pts)) *min-overlap*) (majority-item (gethash Xpt Xpt->0pts)))
                    collect (list Xpt (majority-item (gethash Xpt Xpt->0pts))))
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
    (mapcar
        (lambda (symbol)
            (cond
                ((equal symbol '+X) (x xyz))
                ((equal symbol '-X) (- (x xyz)))
                ((equal symbol '+Y) (y xyz))
                ((equal symbol '-Y) (- (y xyz)))
                ((equal symbol '+Z) (z xyz))
                ((equal symbol '-Z) (- (z xyz)))
                (t (list "ERROR INVALID" x))))
        transform))

(defun apply-transform-offset-l (transform offset l)
    (mapcar
        (lambda (xyz) (mapcar #'+ (apply-transform transform xyz) offset))
        l))

(defun vector- (A B)
    (mapcar #'- A B))

;; correspodences: list of pairs (point in current CRSX; corresponding point in CRS0)
;; returns the transform, e.g. (-X, +Z, -Y) + the offset vector
(defun find-transform-offset (correspodences)
    (let (  (CRSXpts (mapcar #'first correspodences))
            (CRS0pts (mapcar #'second correspodences)))
        (loop for transform in (get-transforms) do
            (let* ( (CRSX-transformed (mapcar 
                                        (lambda (Xpt) (apply-transform transform Xpt)) 
                                        CRSXpts))
                    (offsets (mapcar #'vector- CRS0pts CRSX-transformed)))
                (if (every
                        (lambda (offset) (equal offset (car offsets)))
                        offsets)
                    (return (list transform (car offsets))))))))

(defun merge-lists (a b)
    (remove-duplicates (append a b) :test 'equal))

;; input: list of known points in CRS 0, unmatched windows in different CRS's
;; output: all points in CRS 0
(defun merge-windows (known-points windows offsets)
    (print (list "merge-windows" "known-points:" (length known-points) "windows:" (length windows)))
    (if windows
        (loop for i in (alexandria:iota (length windows)) do
            (let* ( (current-window (nth i windows))
                    (correspodences (get-correspodences known-points current-window)))
                (if (and correspodences)
                    (let* ( (to (find-transform-offset correspodences))
                            (transform (first to))
                            (offset (second to)))
                        (if transform
                            (return 
                                (merge-windows
                                    (merge-lists
                                        known-points
                                        (apply-transform-offset-l transform offset current-window))
                                    (append (subseq windows 0 i) (subseq windows (+ i 1)))
                                    (append offsets (list offset))))
                            nil))
                    nil)))
        (list known-points offsets)))

(defun manhattan-dist (A B)
    (apply #'+
        (mapcar 
            #'abs 
            (mapcar #'- A B))))

(defun problem2 (offsets)
    (if (= 2 (length offsets))
        (apply #'manhattan-dist offsets)
        (max
            (apply 
                #'max
                (mapcar
                    (lambda (off) (manhattan-dist (car offsets) off))
                    (cdr offsets)))
            (problem2 (cdr offsets)))))

(let*
    (
        (data (load-data))
        (result (merge-windows (car data) (cdr data) (list (list 0 0 0))))
        (beacons (first result))
        (offsets (second result))
    )
    (print (list "Number of datas:" (length data)))
    (print (length beacons))
    (print (problem2 offsets))
)

;; A wrong: 311 (too low); 325 (too low); 347 (too low); correct: 491
