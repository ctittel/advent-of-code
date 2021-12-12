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


(defun get-all-paths (graph start end forbidden cache nextnodes-fn)
    (if (gethash (list start end forbidden) cache)
        (gethash (list start end forbidden) cache)
        (setf   (gethash (list start end forbidden) cache)
                (get-all-paths-aux graph start end forbidden cache nextnodes-fn))))

(defun sort-aux (forbidden)
    (sort forbidden #'> :key #'length))

(defun get-all-paths-aux (graph start end forbidden cache nextnodes-fn)
    (cond ((equal start end) 1)
        (t (let ( (nodes (funcall nextnodes-fn graph start forbidden))
                (new-forbidden (if (is-small-cave start) (sort-aux (append forbidden (list start))) forbidden)))
            ;; (print (list start end nodes forbidden))
            (apply #'+ (mapcar 
                        (lambda (next) 
                            (get-all-paths 
                                graph
                                next 
                                end 
                                new-forbidden
                                cache
                                nextnodes-fn))
                        nodes))))))

(defun next-nodes-A (graph node forbidden)
    (remove-if  (lambda (x) (position x forbidden :test 'equal)) 
                (gethash node graph)))

; Problem 1: 4495
(let ((graph (build-graph (load-data))))
    ;; (print graph)
    ;; (print (alexandria:hash-table-alist graph))
    ;; (print (alexandria:hash-table-plist graph))
    (print (get-all-paths graph "end" "start" (list) (make-hash-table :test 'equal) #'next-nodes-A))

)
