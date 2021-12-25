(load "~/quicklisp/setup.lisp")
(require "cl-heap")
(require "alexandria")

(defun construct-path (prev-d current)
    (if current
        (append (construct-path prev-d (gethash current prev-d)) (list current))
        nil))

(defun astar (initial-state is-goal get-actions next-state get-cost heuristic-fn print? with-total-cost?)
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
                    (if with-total-cost?
                        (return (list (construct-path prev current) (gethash current gscore)))
                        (return (construct-path prev current)))
                    (loop for action in (funcall get-actions current) do
                        (let* ( (nstate (funcall next-state current action))
                                (tenative (+ (gethash current gscore) (funcall get-cost current action))))
                            (if (or (null (gethash nstate gscore))
                                    (< tenative (gethash nstate gscore)))
                                (let()
                                    (setf (gethash nstate gscore) tenative)
                                    (setf (gethash nstate prev) current)
                                    (cl-heap:enqueue Q nstate 
                                        (+ tenative
                                            (funcall heuristic-fn nstate))))))))))))

;; -------------- Graph and Path finding stuff

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
        (lambda (x) 0) ; heuristic
        nil ; print?
        nil)))) ; with-actions?

(defun get-path (start stop)
    (if (null (gethash (list start stop) path-cache))
        (if (gethash (list stop start) path-cache)
            (setf (gethash (list start stop) path-cache) (reverse (gethash (list stop start) path-cache)))
            (setf (gethash (list start stop) path-cache) (get-path-aux start stop)))
    (gethash (list start stop) path-cache)))

;; --- Problem stuff

(defparameter *goals* 
    (list
        (list 'A1 'A2)
        (list 'A1 'A2)
        (list 'B1 'B2)
        (list 'B1 'B2)
        (list 'C1 'C2)
        (list 'C1 'C2)
        (list 'D1 'D2)
        (list 'D1 'D2)))
(defparameter *costs* (list 1 1 10 10 100 100 1000 1000))

(defun path-cost (agent path)
    (*  (length (cdr path))
        (nth agent *costs*)))

(defun path-free (state path)
    (every
        (lambda (node) (not (position node state)))
        (cdr path)))

(defparameter *initial-state*
    (list 'B1 'C1 'A2 'B2 'C2 'D1 'A1 'D2))

(defun agent-at-goal (state agent)
    (position (nth agent state) (nth agent *goals*)))

(defun get-agent-paths (state agent)
    (remove-if-not
        (lambda (path) (path-free state path))
        (if (numberp (nth agent state))
            (let* ( (agents-on-goals (mapcar
                                        (lambda (gnode) (position gnode state))
                                        (nth agent *goals*)))
                    (correct-agents-at-goals? (every
                                                (lambda (ag) (or (null ag) (agent-at-goal state ag)))
                                                agents-on-goals)))
                (cond   ((equal agents-on-goals (list nil nil)) (list (get-path (nth agent state) (first (nth agent *goals*)))))
                        (correct-agents-at-goals? (list (get-path (nth agent state) (second (nth agent *goals*)))))
                        (t nil)))
            (mapcar
                (lambda (node) (get-path (nth agent state) node))
                (list 1 2 4 6 8 10 11)))))

(defun get-state-actions (state)
    (apply 
        #'append
        (mapcar
            (lambda (agent) (mapcar 
                                (lambda (path) (list agent (car (last path)) (path-cost agent path)))
                                (get-agent-paths state agent)))
            (alexandria:iota (length state)))))

(defun problem-next-state (state action)
    (append 
        (subseq state 0 (first action))
        (list (second action))
        (subseq state (+ (first action) 1))))

(defun problem-heuristic (state)
    (apply #'+  
        (loop for i in (alexandria:iota (length state)) collect
            (if (position (nth i state) (nth i *goals*))
                0
                (path-cost i (get-path (nth i state) (second (nth i *goals*))))))))

(print (astar 
            *initial-state* 
            (lambda (state) (every 
                                (lambda (i) 
                                    (position (nth i state) (nth i *goals*))) 
                                (alexandria:iota (length state)))) ; is-goal fn
            #'get-state-actions 
            #'problem-next-state 
            (lambda (state action) (third action))
            #'problem-heuristic
            t
            t))

; state space: list (agent-pos)
; action space: list (agent goto-node cost)
