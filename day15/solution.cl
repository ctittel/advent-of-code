(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")
(require "cl-heap")

(defun load-grid() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
        (mapcar (lambda (line) (mapcar #'digit-char-p (coerce line 'list))) 
                lines)))

; ---

(defparameter grid (load-grid))
(defparameter grid-height (length grid))
(defparameter grid-width (length (car grid)))

(defun x (node) (nth 0 node))
(defun y (node) (nth 1 node))

(defun is-valid-node (node)
    (and    (< (y node) grid-height)
            (< (x node) grid-width)
            (>= (y node) 0)
            (>= (x node) 0)))

(defun get-neighbors (node)
    (remove-if-not #'is-valid-node
        (list
            (list (x node) (- (y node) 1))
            (list (x node) (+ (y node) 1))
            (list (- (x node) 1) (y node))
            (list (+ (x node) 1) (y node)))))

(defun get-cost (node)
    (nth (x node) (nth (y node) grid)))

(defun heuristic (node goal)
    (+  (- (x goal) (x node)
        (- (y goal) (y node)))))

(defun astar (start goal)
    (let* 
        (
            (openset (make-instance 'cl-heap:priority-queue))
            (gscore (make-hash-table :test 'equal))
            ; (fscore (make-hash-table :test 'equal))
        )
        (setf (gethash start gscore) 0)
        (cl-heap:enqueue openset start 0)
        (loop do ;(not (cl-heap:is-empty-heap-p openset)) do
            (let ((current (cl-heap:dequeue openset)))
                ; (print current)
                (if (equal current goal) (return (gethash current gscore)) nil)
                (loop for neighbor in (get-neighbors current) do
                    (let ((tenative (+ (gethash current gscore) (get-cost neighbor))))
                        ; (print tenative)
                        (if (or (null (gethash neighbor gscore)) 
                                (< tenative (gethash neighbor gscore)))
                            (let()
                                (setf (gethash neighbor gscore) tenative)
                                ; (setf (gethash neighbor fscore) (+ tenative (heuristic neighbor)))
                                (cl-heap:enqueue openset neighbor (+ tenative (heuristic neighbor goal)))
                            )
                            nil)))))))

(print (astar (list 0 0) (list (- grid-width 1) (- grid-height 1))))

; Problem 2
(defun new-grid (grid)
    (apply #'append
        (loop for i in (alexandria:iota 5) collect
            (apply #'mapcar #'append
                (loop for j in (alexandria:iota 5) collect
                    (loop for row in grid collect
                        (loop for x in row collect
                            (let ((newx (+ x i j)))
                                (if (> newx 9) (mod newx 9) newx)))))))))

(setq grid (new-grid grid))
(setq grid-height (length grid))
(setq grid-width (length (car grid)))

(print (astar (list 0 0) (list (- grid-width 1) (- grid-height 1))))
