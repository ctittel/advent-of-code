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
        (list 'a1 (list 'A1 'A2))
        (list 'a2 (list 'A1 'A2))
        (list 'b1 (list 'B1 'B2))
        (list 'b2 (list 'B1 'B2))
        (list 'c1 (list 'C1 'C2))
        (list 'c2 (list 'C1 'C2))
        (list 'd1 (list 'D1 'D2))
        (list 'd2 (list 'D1 'D2))))
(defparameter *goals-dict* (make-hash-table :test 'equal))
(loop for (agent goals) in *goals* do
    (setf (gethash agent *goals-dict*) goals))

(defparameter *costs*
    (list
        (cons 'a 1)
        (cons 'b 10)
        (cons 'c 100)
        (cons 'd 1000)))

;; (print (get-path 'A1 9))

(defun is-free (state node)
    (let ((occupied (mapcar #'second state)))
        (not (position node occupied))))

(defun path-free (state path)
    (every
        (lambda (node) (is-free state node))
        (cdr path)))

(defparameter *initial-state*
    (list
        (list 'a1 'B1)
        (list 'a2 'C1)
        (list 'b1 'A2)
        (list 'b2 'B2)
        (list 'c1 'C2)
        (list 'c2 'D1)
        (list 'd1 'A1)
        (list 'd2 'D2)))

(defun get-node (state agent)
    (nth
        (position agent (mapcar #'first state))
        (mapcar #'second state)))

(defun at-goal-node (state agent)
    (position (get-node state agent) (gethash agent *goals-dict*)))

(defparameter *agents* (list 'a1 'a2 'b1 'b2 'c1 'c2 'd1 'd2))

(defun is-goal-state (state)
    (every
        (lambda (agent) (at-goal-node state agent))
        *agents*))

(defparameter *state-costs* (make-hash-table :test 'equal))
;; (defun next-states (state)
;;     (loop for agent in  collect
        
;; )

;; (defun get-state-cost (state1 state2)

;; )

;; (print (is-free *initial-state* 'A1))
;; (print (is-free *initial-state* 'D2))
;; (print (is-free *initial-state* 1))
;; (print (is-free *initial-state* 7))

;; (print (path-free
;;             *initial-state*
;;             (get-path 'A1 9)))

;; (print (path-free
;;             *initial-state*
;;             (get-path 1 9)))
