(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")

(defun load-data() 
        (let* ( (data (alexandria:read-file-into-string "test.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
            (reduce
                (lambda (total x) (parse-line total x))
                lines
                :initial-value (list nil 0 0 0))))

; state: i_w, x current subtree, y current subtree, z tree

(defun op-to-fn (cmd)
    (cond 
        ((equal cmd "add") #'+)
        ((equal cmd "mul") #'*)
        ((equal cmd "div") #'/) ; (lambda (a b) (floor (/ a b))))
        ((equal cmd "eql") #'=))) ; (lambda (a b) (if (= a b) 1 0)))))

(defun parse-op (state op a b)
    ;; (print (list "parse-op" state op a b))
    (let* ( (aa (get-subtree state a))
            (bb (get-subtree state b)))
        (set-subtree
            state
            a
            (cond
                ((equal op "add") (list #'+ aa bb))
                ((equal op "mul") (list #'/ aa bb))
                ((equal op "div") (list #'floor (list #'/ aa bb)))
                ((equal op "eql") (list #'= aa bb))
                ((equal op "mod") (list #'rem aa bb))
                (t (error "HERE"))))))

(defun get-subtree (state var)
    ;; (list "get-subtree" (last state) var))
    (cond 
        ((equal var "w") (nth 0 state))
        ((equal var "x") (nth 1 state))
        ((equal var "y") (nth 2 state))
        ((equal var "z") (nth 3 state))
        (t (parse-integer var))))

(defun copy-replace (lst i new)
    (append (subseq lst 0 i) (list new) (subseq lst (+ i 1))))

(defun set-subtree (state var new)
    (cond 
        ((equal var "w") (copy-replace state 0 new))
        ((equal var "x") (copy-replace state 1 new))
        ((equal var "y") (copy-replace state 2 new))
        ((equal var "z") (copy-replace state 3 new))
        (t (error "here"))))

(defun parse-line (state line)
    ;; (print (list "parse-line" state line))
    (let* ( (tokens (split-sequence:split-sequence #\   line :remove-empty-subseqs t)))
        (if (equal (car tokens) "inp") 
            (set-subtree state "w" (gensym "w"))
            (apply #'parse-op (append (list state) tokens)))))

(print (load-data))
;; (load-data)
