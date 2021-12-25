(load "~/quicklisp/setup.lisp")
(require "cl-heap")

;; --- General
(defun construct-path (prev-d current)
    ;; (print (list "construt-path" prev-d current))
    (if current
        (append (construct-path prev-d (gethash current prev-d)) (list current))
        nil))

; TODO: remove item from Q before it is added again
; is-goal: state -> bool
; get-actions: state -> list of actions
; next-state: state action -> state
; get-cost: state -> float
; heuristic-fn: state -> lower bound on cost
(defun astar (initial-state is-goal get-actions next-state get-cost heuristic-fn print?)
    (let* ( (gscore (make-hash-table :test 'equal))
            (prev (make-hash-table :test 'equal))
            (Q (make-instance 'cl-heap:priority-queue)))
        (setf (gethash initial-state gscore) 0)
        (cl-heap:enqueue Q initial-state 0)
        (loop while (> (cl-heap:queue-size Q) 0) do
            (let ((current (cl-heap:dequeue Q)))
                (if print?
                    (print (list "state" current "actions" (funcall get-actions current))))
                (if (funcall is-goal current)
                    (return (construct-path prev current))
                    (loop for action in (funcall get-actions current) do
                        (let* ( (nstate (funcall next-state current action))
                                (tenative (+ (gethash current gscore) (funcall get-cost current action))))
                            ;; (print tenative)
                            (if (or (null (gethash nstate gscore))
                                    (< tenative (gethash nstate gscore)))
                                (let()
                                    (setf (gethash nstate gscore) tenative)
                                    (setf (gethash nstate prev) current)
                                    ; (setf (gethash neighbor fscore) (+ tenative (heuristic neighbor)))
                                    (cl-heap:enqueue Q nstate 
                                        (+ tenative
                                            (funcall heuristic-fn nstate))))))))))))
;; --------------

(defun map-from-adjacencylist (adj)
    (let ((m (make-hash-table :test 'equal)))
        (loop for (a b) in adj do
            (setf (gethash a m) (append (gethash a m) (list b)))
            (setf (gethash b m) (append (gethash b m) (list a))))
        m))

(defparameter *graph*
    (map-from-adjacencylist
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
            (list 'D2 9))))

(defparameter path-cache (make-hash-table :test 'equal))
(defun get-path-aux (start goal)
    (car (last (astar 
        (list start)
        (lambda (state) (equal (car (last state)) goal)) ; is-goal fn
        (lambda (current) (gethash (car (last current)) *graph*)) ; get-actions
        (lambda (state action) (append state (list action))) ; next-state
        (lambda (state action) 1) ; get-cost
        (lambda (x) 0)
        nil)))) ; heuristic

(defun get-path (start stop)
    (if (null (gethash (list start stop) path-cache))
        (if (gethash (list stop start) path-cache)
            (setf (gethash (list start stop) path-cache) (reverse (gethash (list stop start) path-cache)))
            (setf (gethash (list start stop) path-cache) (get-path-aux start stop)))
    (gethash (list start stop) path-cache)))

(defun agent-goals (agent)
    (cond 
        ((equal (first agent) 'a) (list 'A1 'A2))
        ((equal (first agent) 'b) (list 'B1 'B2))
        ((equal (first agent) 'c) (list 'C1 'C2))
        ((equal (first agent) 'd) (list 'D1 'D2))))

(defun agent-cost (agent)
    (cond 
        ((equal (first agent) 'a) 1)
        ((equal (first agent) 'b) 10)
        ((equal (first agent) 'c) 100)
        ((equal (first agent) 'd) 1000)))

(defun path-free (state path)
    (let ((occupied (mapcar #'second state)))
        (every
            (lambda (node) (not (position node occupied)))
            (cdr path))))

;; initial-state is-goal get-actions next-state get-cost

(defparameter *initial-state*
    (list
        (list (list 'a 1) 'B1)
        (list (list 'a 2) 'C1)
        (list (list 'b 1) 'A2)
        (list (list 'b 2) 'B2)
        (list (list 'c 1) 'C2)
        (list (list 'c 2) 'D1)
        (list (list 'd 1) 'A1)
        (list (list 'd 2) 'D2)))

(defun get-agent-pos (state agent)
    (nth
        (position agent (mapcar #'first state))
        (mapcar #'second state)))

(defun get-agent-at (state node)
    (let ((i (position node (mapcar #'second state))))
        (if i
            (nth i (mapcar #'first state))
            nil)))

(defun get-state-actions (state)
    (remove-if-not
        (lambda (p) (path-free state p))
        (apply #'append
            (loop for (agent pos) in state collect
                (if (numberp pos)
                    (let* ((agents-on-gnodes (mapcar 
                                                (lambda (n) (get-agent-at state n)) 
                                                (agent-goals agent))))
                        (if (every 
                                (lambda (ag) (or (null agent) (equal (first ag) (first agent))))
                                agents-on-gnodes)
                            (mapcar
                                (lambda (goal) (list agent (get-path pos goal)))
                                (agent-goals agent))
                            nil))
                    (mapcar
                        (lambda (x) (list agent (get-path pos x)))
                        (list 1 2 4 6 8 10 11)))))))

(defun problem-next-state (state action)
    (let (  (agent (first action))
            (path (second action))
            (i (position (first action) (mapcar #'first state))))
        ;; (print (list "agent" agent "path" path "i" i))
        (append 
            (subseq state 0 i)
            (list (list agent (first (last path))))
            (subseq state (+ i 1)))))

(defun problem-get-cost (state action)
    (let* ( (agent (first action))
            (path (second action)))
        (* (- (length path) 1) (agent-cost agent))))

(defun problem-heuristic (state)
    (apply #'+  
        (loop for (agent pos) in state collect
            (if (position agent (agent-goals agent))
                0
                (* (agent-cost agent)
                    (- (length (get-path 
                                    pos
                                    (second (agent-goals agent))))
                        1))))))

;; initial-state is-goal get-actions next-state get-cost heuristic-fn

(print (astar 
            *initial-state* 
            (lambda (state) (every 
                                (lambda (agent-state) (position (second agent-state) (agent-goals (first agent-state)))) 
                                state)) ; is-goal fn
            #'get-state-actions 
            #'problem-next-state 
            #'problem-get-cost
            #'problem-heuristic
            t))
