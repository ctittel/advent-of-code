(load "~/quicklisp/setup.lisp")
(require "cl-heap")

;; --- General
(defun construct-path (prev-d current)
    ;; (print (list "construt-path" prev-d current))
    (if current
        (append (construct-path prev-d (gethash current prev-d)) (list current))
        nil))

(defun dijkstra (start is-goal get-neighbors get-cost)
    (let* 
        (
            (dist (make-hash-table :test 'equal))
            (prev (make-hash-table :test 'equal))
            (Q (make-instance 'cl-heap:priority-queue))
        )
        (setf (gethash start dist) 0)
        (cl-heap:enqueue Q start 0)
        (loop while (> (cl-heap:queue-size Q) 0) do
            (let ((current (cl-heap:dequeue Q)))
                ;; (print Q)
                (if (funcall is-goal current)
                    (return (construct-path prev current))
                    nil)
                (loop for neighbor in (funcall get-neighbors current) do
                    (let ((tenative (+ (gethash current dist) (funcall get-cost current neighbor))))
                        ;; (print tenative)
                        (if (or (null (gethash neighbor dist)) 
                                (< tenative (gethash neighbor dist)))
                            (let()
                                (setf (gethash neighbor dist) tenative)
                                (setf (gethash neighbor prev) current)
                                ; (setf (gethash neighbor fscore) (+ tenative (heuristic neighbor)))
                                (cl-heap:enqueue Q neighbor tenative))
                            nil)))))))
;; --------------


; state: list (agent, pos)

(defparameter *adj* 
    (list
        (list 1 2)
        (list 2 3)
        (list 3 4)
        (list 4 5)
        (list 5 6)
        (list 6 7)
        (list 7 8)
        (list 8 9)
        (list 9 10)
        (list 10 11)
        (list 'A1 'A2)
        (list 'A2 3)
        (list 'B1 'B2)
        (list 'B2 5)
        (list 'C1 'C2)
        (list 'C2 7)
        (list 'D1 'D2)
        (list 'D2 9)))

(defparameter *adjmap* (make-hash-table :test 'equal))
(loop for (a b) in *adj* do
    (setf (gethash a *adjmap*) (append (gethash a *adjmap*) (list b)))
    (setf (gethash b *adjmap*) (append (gethash b *adjmap*) (list a))))

(defun get-graph-neighbors (current)
    (gethash current *adjmap*))

(defparameter path-cache (make-hash-table :test 'equal))
(defun get-path-aux (start goal)
    (dijkstra 
        start
        (lambda (node) (equal node goal)) ; is-goal fn
        #'get-graph-neighbors
        (lambda (a b) 1))) ; get-cost fn

(defun get-path (start stop)
    (if (null (gethash (list start stop) path-cache))
        (setf (gethash (list start stop) path-cache) (get-path-aux start stop)))
    (gethash (list start stop) path-cache))

(defparameter *goals*
    (list
        (cons 'a (cons 'A1 'A2))
        (cons 'b (cons 'B1 'B2))
        (cons 'c (cons 'C1 'C2))
        (cons 'd (cons 'D1 'D2))))

(defparameter *costs*
    (list
        (cons 'a 1)
        (cons 'b 10)
        (cons 'c 100)
        (cons 'd 1000)))

(print (get-path 'A1 9))
