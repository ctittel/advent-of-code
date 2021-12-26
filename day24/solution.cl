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
                :initial-value (list (list 'w 0) 0 0 0))))

; state: i_w, x current subtree, y current subtree, z tree

(defun parse-op (state op a b)
    ;; (print (list "parse-op" state op a b))
    (let* ( (aa (get-subtree state a))
            (bb (get-subtree state b)))
        (set-subtree
            state
            a
            (cond
                ((equal op "add") (list 'add aa bb))
                ((equal op "mul") (list 'mul aa bb))
                ((equal op "div") (list 'div aa bb))
                ((equal op "eql") (list 'eql aa bb))
                ((equal op "mod") (list 'rem aa bb))
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
            (set-subtree state "w" 
                (list 'w (+ 1 (second (get-subtree state "w")))))
            (apply #'parse-op (append (list state) tokens)))))

;; ---- SIMPLIFYING

; ADD MUL DIV MOD EQL

(defun is-w (l)
    (equal (first l) 'w))

(defun simplify-matcher (x) 
    (trivia:match x
        ((or (list 'mul 0 _) (list 'mul _ 0)) 0)
        ((list 'div a 1) a)
        ((or (list 'add 0 a) (list 'add a 0)) a)
        ((or (list 'mul 1 a) (list 'mul a 1)) a)
        ((trivia:guard 
            (list 'mul a b)
            (and (numberp a) (numberp b))) (* a b))
        ((trivia:guard 
            (list 'add a b)
            (and (numberp a) (numberp b))) (+ a b))
        ((trivia:guard 
            (list 'rem a b)
            (and (numberp a) (numberp b))) (rem a b))
        ((trivia:guard 
            (list 'div a b)
            (and (numberp a) (numberp b))) (floor (/ a b)))
        ((trivia:guard 
            (list 'eql a b)
            (and (numberp a) (numberp b))) (if (= a b) 1 0))
        ((trivia:guard 
            (or (list 'eql a w) (list 'eql w a))
            (and (numberp a) (>= a 10) (is-w w))) 0)
        ((or   (list 'mul a (list 'add b c)) 
               (list 'mul (list 'add b c) a))
            (list 'add (list 'mul a b) (list 'mul a c)))
        ((trivia:guard 
            (list 'rem a b)
            (> b (upper-estimate a)))
            a)
        ((trivia:guard 
            (or (list 'add (list 'add rest a) b)
                (list 'add (list 'add a rest) b)
                (list 'add b (list 'add a rest))
                (list 'add b (list 'add rest a)))
            (and    (numberp a)
                    (numberp b)))
            (list 'add rest (+ a b)))
        ((trivia:guard 
            (or (list 'eql a b) (list 'eql b a))
            (>  (lower-estimate a) (upper-estimate b)))
            0)
        ((list* _) x)))

(defun simplify (x)
    (if (listp x)
        (simplify-matcher (mapcar #'simplify x))
        x))

(defun lower-matcher (x)
    (trivia:match x
        ((trivia:guard
            (or (list 'mul a rest)
                (list 'mul rest a))
            (and (numberp a) (< a 0)))
            (* a (upper-estimate rest)))
        ((trivia:guard
            (or (list 'mul a rest)
                (list 'mul rest a))
            (and (numberp a) (> a 0)))
            (* a (lower-estimate rest)))
        ((list 'w i) 1)
        ((list* _) x)))

(defun upper-matcher (x)
    (trivia:match x
        ((trivia:guard
            (or (list 'mul a rest)
                (list 'mul rest a))
            (and (numberp a) (< a 0)))
            (* a (lower-estimate rest)))
        ((trivia:guard
            (or (list 'mul a rest)
                (list 'mul rest a))
            (and (numberp a) (> a 0)))
            (* a (upper-estimate rest)))
        ((list 'w i) 9)
        ((list* _) x)))

(defun upper-estimate (x)
    (if (listp x)
        (simplify (upper-matcher (mapcar #'upper-estimate (simplify x))))
        x))

(defun lower-estimate (x)
    (if (listp x)
        (simplify (lower-matcher (mapcar #'lower-estimate (simplify x))))
        x))

(defparameter *state* (load-data))
(defparameter *z* (get-subtree *state* "z"))
(print (simplify (simplify (simplify (simplify *z*)))))
