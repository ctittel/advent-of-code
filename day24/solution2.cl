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
            (cond
                ((equal op "add") (list 'add aa bb))
                ((equal op "mul") (list 'mul aa bb))
                ((equal op "div") (list 'div aa bb))
                ((equal op "mod") (list 'mod aa bb));; mod a b == a - ((a / b) * b)
                    ;; (list 'add  aa 
                    ;;             (list 'mul 
                    ;;                 (list 'mul
                    ;;                     (list 'div aa bb)
                    ;;                     bb)
                    ;;                 -1)))
                ((equal op "eql") (list 'eql aa bb))
                (t (error "HERE"))))))

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
    (let* ( (tokens (split-sequence:split-sequence #\   line :remove-empty-subseqs t)))
        (if (equal (car tokens) "inp") 
            (set-subtree state "w" (+ 1 (nth 0 state)))
            (apply #'parse-op (append (list state) tokens)))))

;; ---- SIMPLIFYING

(defun simplify-patterns (cache subst tree)
    (trivia:match tree
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
                ((equal op 'div) (floor (/ a b)))
                ((equal op 'eql) (if (= a b) 1 0))
                ((equal op 'mod) (rem a b))
                (t (error "Unknown operation"))))
        ((trivia:guard
            (or (list 'eql (list 'w i) a) (list 'eql a (list 'w i)))
            (and (numberp a) (> a 9)))
            0)
        ((trivia:guard
            (or (list 'add (list 'add a rest) b) 
                (list 'add b (list 'add a rest)) 
                (list 'add b (list 'add rest a)) 
                (list 'add (list 'add rest a) b))
            (and (numberp a) (numberp b)))
            (list 'add (+ a b) rest))
        ;; ((trivia:guard 
        ;;     (list 'eql a b) 
        ;;     (or (> (lower-bound a setW) (upper-bound b setW)) 
        ;;         (> (lower-bound b setW) (upper-bound a setW)))) 0)
        ((trivia:guard (list 'w i) (nth i subst))
            (nth i subst))
        ((list* _) nil)))

(defun simplify-aux (cache subst tree)
    (if (or (null tree) (not (listp tree)))
        tree
        (let* ( (leafs (mapcar
                        (lambda (leaf) (simplify cache subst leaf))
                        (cdr tree)))
                (ntree (append (list (car tree)) leafs))
                (matched (simplify-patterns cache subst ntree)))
        (if matched
            (simplify-aux cache subst matched)
            ntree))))

(defun simplify (cache subst tree)
    (let ((key (list subst tree)))
        (if (gethash key cache)
            (gethash key cache)
            (setf (gethash key cache) (simplify-aux cache subst tree)))))

; ADD MUL DIV MOD EQL

(load-data)

(defparameter *cache* (make-hash-table :test 'equal))
(defparameter *subst* (list 9 9 9 9 9 9 9 9 9 9 9 9 9 9))
(defparameter *data* (fourth (load-data)))

(loop for i in (alexandria:iota 9 :start 1) do
    (print i)
    (print (simplify    
                *cache*
                (list 9 9 5 6 9 1 9 3 4 nil 9 7 9 9 );; *subst*
                *data*)))
