(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data()
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
        (mapcar (lambda (line) (split-sequence:split-sequence #\- line)) 
                lines)))

; --- Done loading ---

(defun build-graph (data)
    (let ((graph (make-hash-table :test 'equal)))
        (loop for edge in data do
            (let ()
                (setf (gethash (nth 0 edge) graph) (append (list (nth 1 edge)) (gethash (nth 0 edge) graph)))
                (setf (gethash (nth 1 edge) graph) (append (list (nth 0 edge)) (gethash (nth 1 edge) graph)))))
        graph))

(defun is-small-cave (node)
    (every #'lower-case-p (coerce node 'list)))

(defun sort-aux (forbidden)
    (sort forbidden #'> :key #'length))

(defun get-all-paths (graph current end forbidden nextnodes-fn)
    (cond ((equal current end) 1)
        (t (let* (  (new-forbidden (if  (is-small-cave current) 
                                    (sort-aux (append forbidden (list current))) 
                                    forbidden))
                    (nodes (funcall nextnodes-fn graph current new-forbidden)))
            (apply #'+ (mapcar 
                        (lambda (next) 
                            (get-all-paths 
                                graph
                                next 
                                end 
                                new-forbidden
                                nextnodes-fn))
                        nodes))))))

(defun next-nodes-A (graph node forbidden)
    (remove-if  (lambda (x) (position x forbidden :test 'equal)) 
                (gethash node graph)))

(defun next-nodes-B (graph node forbidden)
    (if (= 0 (length forbidden))
        (gethash node graph)
        (let* ((counts (mapcar (lambda (x) (count x forbidden :test 'equal)) forbidden))
                (max-count (apply #'max counts)))
            (if (> max-count 1)
                (next-nodes-A graph node forbidden)
                (remove-if (lambda (x) (equal x "start")) (gethash node graph))))))

(let ((graph (build-graph (load-data))))
    (print (get-all-paths graph "start" "end" (list) #'next-nodes-A))
    (print (get-all-paths graph "start" "end" (list)  #'next-nodes-B))
)
