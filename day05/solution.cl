(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "alexandria")
(require "split-sequence")
(require "alexandria")

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (load-line line))))

(defun load-data () (load-file "input.txt"))

(defun load-line (line) 
    (let* ( (splitted-line (split-sequence:split-sequence #\  line))
            (first-coord (nth 0 splitted-line))
            (second-coord (nth 2 splitted-line))
        )
        (mapcar (lambda (string-coords) 
                            (mapcar #'parse-integer (split-sequence:split-sequence #\, string-coords)))
                (list first-coord second-coord))
))

;; --- Below the actual problem ---

(defun line-start (line) (nth 0 line))
(defun line-end (line) (nth 1 line))

(defun get-x (point) (nth 0 point))
(defun get-y (point) (nth 1 point))

(defun is-horizontal (line) (= (get-y (line-start line)) (get-y (line-end line))))
(defun is-vertical (line) (= (get-x (line-start line)) (get-x (line-end line))))
(defun is-diagonal (line) (and (not (is-horizontal line)) (not (is-vertical line))))

;; (defun get-max-point (lines)
;;     (let* ((all-points (reduce #'append lines :initial-value '()))
;;             (all-x (mapcar #'get-x all-points))
;;             (all-y (mapcar #'get-y all-points)))
;;         (list (apply #'max all-x) (apply #'max all-y))))

;; (defun init-map (max-x max-y)
;;     (make-list (+ 1 max-y) :initial-element (make-list (+ 1 max-x) :initial-element 0) ))

(defun get-map-val (*map* point)
    (let ((val (gethash point *map*)))
        (if val val 0)))

(defun update-map (*map* point)
    (setf (gethash point *map*) (+ 1 (get-map-val *map* point))))

(defun add-lines-to-map (*map* lines)
    (loop for line in lines do
        (loop for point in (get-line-points line) do (update-map *map* point))))

(defun get-range (start stop)
    (let ((smaller (min start stop))
            (larger (max start stop)))
    (alexandria:iota (+ 1 (- larger smaller)) :start smaller)))

(defun get-line-points (line)
    (let* ((start (line-start line))
            (end (line-end line)))
        (cond ((is-horizontal line) (mapcar (lambda (x) (list x (get-y start)))
                                                (get-range (get-x start) (get-x end))))
                ((is-vertical line) (mapcar (lambda (y) (list (get-x start) y))
                                                (get-range (get-y start) (get-y end))))
            (t "fail")
        )
))

(defun problem1 (lines)
    (let* ((the-map (make-hash-table :test 'equal))
            (no-diagonals (remove-if #'is-diagonal lines)))
        (add-lines-to-map the-map no-diagonals)
        (reduce (lambda (total x) (if (>= x 2) (+ 1 total) total)) (alexandria:hash-table-values the-map) :initial-value 0)))


;; (print (is-diagonal (list (list 10 1) (list 0 10))))

;; (setq max-point (get-max-point (load-data)))
;; (setq the-map (make-hash-table :test 'equal))
;; (setq initial-map (apply #'init-map max-point))

;; (print max-point)
;; (print the-map)
;; (print (get-map-val the-map (list 10 10)))
;; (update-map the-map (list 10 10))
;; (update-map the-map (list 10 10))
;; (update-map the-map (list 10 10))
;; (update-map the-map (list 10 10))
;; (print (get-map-val the-map (list 10 10)))
;; (print the-map)
;; (print (alexandria:hash-table-keys the-map))

;; (print (get-line-points (list (list 10 0) (list 100 0)) ))
;; (print (get-line-points (list (list 100 0) (list 10 0)) ))
;; (print (get-line-points (list (list 0 33) (list 0 0)) ))
;; (print (length (load-data)))

(print (problem1 (load-data)))
