;; NOTE:     This was my first attempt at solving puzzle 
;;           The idea was to simplify the expression using pattern matching and then hopefully find a simple closed form solution
;;           This approach didn't work - see solution2.cl for an approach that works


(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")
(require "trivia")

;; ----- PARSING

(defun load-data() 
        (let* ( (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
            (reduce
                (lambda (total x) (parse-line total x))
                lines
                :initial-value (list 0 0 0 0))))

; state: i_w, x current subtree, y current subtree, z tree

(defun parse-op (state op a b)
    ;; (print (list "parse-op" state op a b))
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
                    (t (error "HERE")))
                nil))))

(defun get-subtree (state var)
    ;; (list "get-subtree" (last state) var))
    (cond 
        ((equal var "w") (list 'w (- (nth 0 state) 1)))
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
            (set-subtree state "w" (+ 1 (nth 0 state)))
            (apply #'parse-op (append (list state) tokens)))))

;; ---- SIMPLIFYING

; ADD MUL DIV MOD EQL

(defun op/ (a b) (floor (/ a b)))
(defun op= (a b) (if (= a b) 1 0))

(defun simplify-matcher (x setW)
    ;; (print (list "simplify-matcher" x))
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
            (or (> (lower-bound a setW) (upper-bound b setW)) 
                (> (lower-bound b setW) (upper-bound a setW)))) 0)
        ((trivia:guard (list 'w i) (< i (length setW)))
            (nth i setW))
        ((list* _) nil)))

(defun upper-matcher (x setW)
    (if (equal (first x) 'w)
        (if (< (second x) (length setW))
            (nth (second x) setW)
            9)
        (let* ( (la (lower-bound (second x) setW))
                (ua (upper-bound (second x) setW))
                (lb (lower-bound (third x) setW))
                (ub (upper-bound (third x) setW)))
            (trivia:match x
                ((list 'add a b) (+ ua ub))
                ((list 'mul a b) 
                    (max (* la lb) (* la ub) (* ua lb) (* ua ub)))
                ((list 'div a b) 
                    (max (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
                ((list 'eql a b) 1)
                ((list* _) nil)))))

; ADD MUL DIV MOD EQL

(defun lower-matcher (x setW)
    (if (equal (first x) 'w)
        (if (< (second x) (length setW))
            (nth (second x) setW)
            1)
        (let* ( (la (lower-bound (second x) setW))
                (ua (upper-bound (second x) setW))
                (lb (lower-bound (third x) setW))
                (ub (upper-bound (third x) setW)))
            (trivia:match x
                ((list 'add a b) (+ la lb))
                ((list 'mul a b) 
                    (min (* la lb) (* la ub) (* ua lb) (* ua ub)))
                ((list 'div a b) 
                    (min (op/ la lb) (op/ la ub) (op/ ua lb) (op/ ua ub)))
                ((list 'eql a b) 0)
                ((list* _) nil)))))

(defun bound-aux (cache matcher x setW)
    ;; (print (list "bound-aux" matcher x))
    (let (  (xx (simplify x setW))
            (key (list x setW)))
        (cond   ((null xx) (error "bound-aux rceived NIL"))
                ((not (listp xx)) xx)
                ((gethash key cache) (gethash key cache))
                (t  (setf   (gethash key cache)
                            (let* ( (z  (funcall matcher xx setW)))
                                (if (null z)
                                    xx
                                    (bound-aux cache matcher z setW))))))))

(defparameter *scache* (make-hash-table :test 'equal))
(defun simplify-aux (x setW)
    (cond 
        ((null x) (error "simplify-aux received NIL"))
        ((not (listp x)) x)
        (t (let ((z (simplify-matcher x setW)))
                (if (null z)
                    x 
                    (simplify z setW))))))

(defun simplify (x setW)
    ;; (print (list "simplify" x))
    (cond   ((null x) (error "simplify received nil"))
            ((gethash (list x setW) *scache*) 
                (gethash (list x setW) *scache*))
            (t (setf    (gethash (list x setW) *scache*)
                        (simplify-aux x setW)))))

(defparameter *ucache* (make-hash-table :test 'equal))
(defun upper-bound (x setW)
    (bound-aux *ucache* #'upper-matcher x setW))

(defparameter *lcache* (make-hash-table :test 'equal))
(defun lower-bound (x setW)
    (bound-aux *lcache* #'lower-matcher x setW))


;; (defparameter test (list 'eql (list 'add 10 -4) (list 'w 1)))
;; (print (lower-bound test))
;; (print (upper-bound test))

(defparameter *state* (load-data))
(defparameter *z* (get-subtree *state* "z"))
;; (print (identity *z*))
(print (lower-bound *z* nil))
(print (upper-bound *z* nil))
;; (print (nth 0 *state*))

;; (defun simplify-all (x setW)
;;     (if (listp x)
;;         (simplify
;;             (mapcar (lambda (xx) (simplify-all xx setW)) x)
;;             setW)
;;         x))

(defparameter w (list 9 9 5 6 9 1 9 3 4 9 9 7 9 9))
(defparameter test (list 'mul (list 'add (list 'w 10) (list 'w 1)) (list 'w 20)))

;; (print (simplify test w))
;; (print (upper-bound test w))
(print (upper-bound *z* w))
(print (lower-bound *z* w))

;; (loop for i in (alexandria:iota (nth 0 *state*)) do

;; (print *z*)
;; (defun find-largest-w (tree w))
