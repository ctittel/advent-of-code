(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")
(require "trivia")

;; ----- PARSING

(defun load-data() 
        (let* ( (data (alexandria:read-file-into-string "test.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
            (reduce
                (lambda (total x) (parse-line total x))
                lines
                :initial-value (list (list 'w 0) 0 0 0))))

; state: i_w, x current subtree, y current subtree, z tree

(defun parse-op (state op a b)
    (print (list "parse-op" state op a b))
    (let* ( (aa (get-subtree state a))
            (bb (get-subtree state b)))
        (set-subtree
            state
            a
            (simplify
                (cond
                    ((equal op "add") (list 'add aa bb))
                    ((equal op "mul") (list 'mul aa bb))
                    ((equal op "div") (list 'div aa bb))
                    ((equal op "mod") ;; mod a b == a - ((a / b) * b)
                        (list 'add  aa 
                                    (list 'mul 
                                        (list 'mul
                                            (list 'div aa bb)
                                            bb)
                                        -1)))
                    ((equal op "eql") (list 'eql aa bb))
                    (t (error "HERE")))))))

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
        ((null new) (error "NEW SUBTREE IS NIL"))
        ((equal var "w") (copy-replace state 0 new))
        ((equal var "x") (copy-replace state 1 new))
        ((equal var "y") (copy-replace state 2 new))
        ((equal var "z") (copy-replace state 3 new))
        (t (error "here"))))

(defun parse-line (state line)
    ;; (print (list "parse-line" state line))
    (let* ( (tokens (split-sequence:split-sequence #\   line :remove-empty-subseqs t)))
        (if (equal (car tokens) "inp") 
            (set-subtree state "w" 
                (list 'w (+ 1 (second (get-subtree state "w")))))
            (apply #'parse-op (append (list state) tokens)))))

;; ---- SIMPLIFYING

; ADD MUL DIV MOD EQL

(defun op/ (a b) (floor (/ a b)))
(defun op= (a b) (if (= a b) 1 0))

(defun simplify-matcher (x)
    (print (list "simplify-matcher" x))
    (print
    (let* ( (la (lower-estimate (second x)))
            (ua (upper-estimate (second x)))
            (lb (lower-estimate (third x)))
            (ub (upper-estimate (third x))))
        (trivia:match x
            ((or (list 'mul 0 _) (list 'mul _ 0)) 0)
            ((list 'div a 1) a)
            ((or (list 'add 0 a) (list 'add a 0)) a)
            ((or (list 'mul 1 a) (list 'mul a 1)) a)
            ((trivia:guard 
                (list op a b)
                (and (numberp a) (numberp b)))
                (cond 
                    ((equal op 'add) (+ a b))
                    ((equal op 'mul) (* a b))
                    ((equal op 'div) (op/ a b))
                    ((equal op 'eql) (op= a b))
                    ((equal op 'mod) (rem a b))
                    (t (error "Unknown operation"))))
            ((trivia:guard (list 'eql a b) (or (> la ub) (> lb ua))) 0)
            ((list* _) nil)))))

(defun upper-matcher (x)
    (let* ( (la (lower-estimate (second x)))
            (ua (upper-estimate (second x)))
            (lb (lower-estimate (third x)))
            (ub (upper-estimate (third x))))
        (trivia:match x
            ((list 'add a b) (+ ua ub))
            ((list 'mul a b) (max (* la lb) (* la ub) (* ua lb) (* ua ub)))
            ((list 'div a b) (max (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
            ((list* _) nil))))

; ADD MUL DIV MOD EQL

(defun lower-matcher (x)
    (let* ( (la (lower-estimate (second x)))
            (ua (upper-estimate (second x)))
            (lb (lower-estimate (third x)))
            (ub (upper-estimate (third x))))
        (trivia:match x
            ((list 'add a b) (+ la lb))
            ((list 'mul a b) (min (* la lb) (* la ub) (* ua lb) (* ua ub)))
            ((list 'div a b) (min (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
            ((list* _) nil))))

(defun estimate-aux (cache matcher x)
    (print (list "estimate-aux" matcher x))
    (cond   ((null x) (error "estimate-aux rceived NIL"))
            ((not (listp x)) x)
            ((gethash x cache) (gethash x cache))
            (t  (setf   (gethash x cache)
                        (let ((z  (funcall matcher x)))
                            (if (null z)
                                x 
                                (estimate-aux cache matcher z)))))))

(defparameter *scache* (make-hash-table :test 'equal))
(defun simplify-aux (x)
    (if (null x) 
        (error "simplify-aux received NIL")
        (let ((z (simplify-matcher x)))
            (if (null z)
                x 
                (simplify z)))))
(defun simplify (x)
    (print (list "simplify" x))
    (cond   ((null x) (error "simplify received nil"))
            ((not (listp x)) x)
            ((equal (first x) 'w) x)
            ((gethash x *scache*) (gethash x *scache*))
            (t (setf    (gethash x *scache*)
                        (simplify-aux x)))))

(defparameter *ucache* (make-hash-table :test 'equal))
(defun upper-estimate (x)
    (cond 
        ((null x) (error "upper-estimate received nil"))
        ((not (listp x)) x)
        ((equal (first x) 'w) 9)
        (t (estimate-aux *ucache* #'upper-matcher x))))

(defparameter *lcache* (make-hash-table :test 'equal))
(defun lower-estimate (x)
    (cond 
        ((null x) (error "lower-estimate received nil"))
        ((not (listp x)) x)
        ((equal (first x) 'w) 1)
        (t (estimate-aux *lcache* #'lower-matcher x))))

(defparameter *state* (load-data))
;; (defparameter *z* (get-subtree *state* "z"))
;; (print (identity *z*))
