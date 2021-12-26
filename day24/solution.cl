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
(defun op= (a b) 
    (let ()
        (print "OP= CALLED")
        (if (= a b) 1 0)))

(defun simplify-matcher (x)
    (print (list "simplify-matcher" x))
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
        ((trivia:guard 
            (list 'eql a b) 
            (or (> (lower-estimate a) (upper-estimate b)) 
                (> (lower-estimate b) (upper-estimate a)))) 0)
        ((list* _) nil)))

(defun upper-matcher (x)
    (if (equal (first x) 'w)
        9
        (let* ( (la (lower-estimate (second x)))
                (ua (upper-estimate (second x)))
                (lb (lower-estimate (third x)))
                (ub (upper-estimate (third x))))
            (trivia:match x
                ((list 'add a b) (+ (upper-estimate a) (upper-estimate b)))
                ((list 'mul a b) (max (* la lb) (* la ub) (* ua lb) (* ua ub)))
                ((list 'div a b) (max (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
                ((list* _) nil)))))

; ADD MUL DIV MOD EQL

(defun lower-matcher (x)
    (if (equal (first x) 'w)
        1
        (let* ( (la (lower-estimate (second x)))
                (ua (upper-estimate (second x)))
                (lb (lower-estimate (third x)))
                (ub (upper-estimate (third x))))
            (trivia:match x
                ((list 'add a b) 
                    (+ (lower-estimate a) (lower-estimate b)))
                ((list 'mul a b) (min 
                    (* (lower-estimate a) (lower-estimate b)) 
                    (* (lower-estimate a) (upper-estimate b)) 
                    (* (upper-estimate a) (lower-estimate b)) 
                    (* (upper-estimate a) (upper-estimate b))))
                ((list 'div a b) (min (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
                ((list* _) nil)))))

(defun estimate-aux (cache matcher x)
    (print (list "estimate-aux" matcher x))
    (let ((xx (simplify x)))
        (cond   ((null xx) (error "estimate-aux rceived NIL"))
                ((not (listp xx)) xx)
                ((gethash xx cache) (gethash xx cache))
                (t  (setf   (gethash xx cache)
                            (let* ( (z  (funcall matcher xx)))
                                (if (null z)
                                    xx 
                                    (estimate-aux cache matcher z))))))))

(defparameter *scache* (make-hash-table :test 'equal))
(defun simplify-aux (x)
    (cond 
        ((null x) (error "simplify-aux received NIL"))
        ((not (listp x)) x)
        (t (let ((z (simplify-matcher x)))
                (if (null z)
                    x 
                    (simplify z))))))

(defun simplify (x)
    (print (list "simplify" x))
    (cond   ((null x) (error "simplify received nil"))
            ((gethash x *scache*) (gethash x *scache*))
            (t (setf    (gethash x *scache*)
                        (simplify-aux x)))))

(defparameter *ucache* (make-hash-table :test 'equal))
(defun upper-estimate (x)
    (estimate-aux *ucache* #'upper-matcher x))

(defparameter *lcache* (make-hash-table :test 'equal))
(defun lower-estimate (x)
    (estimate-aux *lcache* #'lower-matcher x))

;; (defparameter *state* (load-data))

(defparameter test (list 'eq 0 (list 'w 1)))
(print (simplify test))
(print (lower-estimate test))
(print (upper-estimate test))

;; (defparameter *z* (get-subtree *state* "z"))
;; (print (identity *z*))
