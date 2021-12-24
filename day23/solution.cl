(load "~/quicklisp/setup.lisp")
(require "cl-heap")

;; --- General
(defun construct-path (prev-d current)
    ;; (print (list "construt-path" prev-d current))
    (if current
        (append (construct-path prev-d (gethash current prev-d)) (list current))
        nil))

; is-goal: state -> bool
; get-actions: state -> list of actions
; next-state: state action -> state
; get-cost: state -> float
(defun dijkstra (initial-state is-goal get-actions next-state get-cost)
    (let* 
        (   (dist (make-hash-table :test 'equal))
            (prev (make-hash-table :test 'equal))
            (Q (make-instance 'cl-heap:priority-queue)))
        (setf (gethash initial-state dist) 0)
        (cl-heap:enqueue Q initial-state 0)
        (loop while (> (cl-heap:queue-size Q) 0) do
            (let ((current (cl-heap:dequeue Q)))
                ;; (print current)
                (if (funcall is-goal current)
                    (return (construct-path prev current))
                    nil)
                ;; (print (list "state" current "actions" (funcall get-actions current)))
                (loop for action in (funcall get-actions current) do
                    (let* ( (nstate (funcall next-state current action))
                            (tenative (+ (gethash current dist) (funcall get-cost current action))))
                        ;; (print tenative)
                        (if (or (null (gethash nstate dist)) 
                                (< tenative (gethash nstate dist)))
                            (let()
                                (setf (gethash nstate dist) tenative)
                                (setf (gethash nstate prev) current)
                                ; (setf (gethash neighbor fscore) (+ tenative (heuristic neighbor)))
                                (cl-heap:enqueue Q nstate tenative))
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

(defparameter path-cache (make-hash-table :test 'equal))
(defun get-path-aux (start goal)
    (car (last (dijkstra 
        (list start)
        (lambda (state) (equal (car (last state)) goal)) ; is-goal fn
        (lambda (current) (gethash (car (last current)) *adjmap*)) ; get-actions
        (lambda (state action) (append state (list action))) ; next-state
        (lambda (state action) 1))))) ; get-cost
;; initial-state is-goal get-actions next-state get-cost

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

(defun agent-cost (agent)
    (cond 
        ((equal agent 'a1) 1)
        ((equal agent 'a2) 1)
        ((equal agent 'b1) 10)
        ((equal agent 'b2) 10)
        ((equal agent 'c1) 100)
        ((equal agent 'c2) 100)
        ((equal agent 'd1) 1000)
        ((equal agent 'd2) 1000)))

;; (print (get-path 'A1 9))

(defun is-free (state node)
    (let ((occupied (mapcar #'second state)))
        (not (position node occupied))))

(defun path-free (state path)
    (every
        (lambda (node) (is-free state node))
        (cdr path)))

;; initial-state is-goal get-actions next-state get-cost

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

(defun get-agent-node (state agent)
    (nth
        (position agent (mapcar #'first state))
        (mapcar #'second state)))

(defun get-agent-at (state node)
    (nth
        (position node (mapcar #'second state))
        (mapcar #'first state)))

(defun at-goal-node (state agent)
    (position (get-agent-node state agent) (gethash agent *goals-dict*)))

(defparameter *agents* (list 'a1 'a2 'b1 'b2 'c1 'c2 'd1 'd2))

(defun is-goal-state (state)
    (every
        (lambda (agent) (at-goal-node state agent))
        *agents*))

(defun get-state-actions (state)
    (remove-if-not
        (lambda (p) (path-free state p))
        (apply #'append
            (loop for agent in *agents* collect
                (let* ((node (get-agent-node state agent)))
                    (cond   
                            ;; ((at-goal-node state agent) nil)
                            ((numberp node)
                                (mapcar
                                    (lambda (goal) (get-path node goal))
                                    (gethash agent *goals-dict*)))
                            (t (mapcar
                                    (lambda (x) (get-path node x))
                                    (list 1 2 4 6 8 10 11)))))))))

(defun problem-next-state (state action)
    (let* ((agent (get-agent-at state (first action))))
        (append
            (remove-if
                (lambda (agent-node) (equal (first agent-node) agent))
                state)
            (list (list agent (car (last action)))))))

(defun problem-get-cost (state action)
    (let* ((agent (get-agent-at state (first action))))
        (* (- (length action) 1) (agent-cost agent))))

;; initial-state is-goal get-actions next-state get-cost

(print (dijkstra *initial-state* #'is-goal-state #'get-state-actions #'problem-next-state #'problem-get-cost))
