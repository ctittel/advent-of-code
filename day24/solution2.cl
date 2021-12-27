;; How I solved it:
;;      In the input you can identify two types of blocks: 
;;          - PUT blocks which add a letter to z, which contain: `div z 1`
;;          - POP blocks which remove a letter from z, which contain: `div z 26`
;;      It seems to be a LIFA Queue (last in, first out)
;;      Every PUT block corresponds to one POP block, so there is a relationship between two digits
;;      The relationships are the following: 1 & 14; 2 & 13; 3 & 6; 4 & 5; 7 & 8; 9 & 10; 11 & 12
;; Through some trial and error (see the bottom of this file) you can identify the following relationships that must be true for a number to be minimal:
;;  W1 = W14 + 7
;;  W2 = W13 - 7
;;  W3 = W6 + 4
;;  W4 = W5 - 3
;;  W7 = W8 + 6
;;  W9 = W10 - 5
;;  W11 = W12 - 2
;; With these simple relationships identified it is now easy to plug together the minimal / maximal serial number


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
        ((trivia:guard (list 'w i) (nth i subst))
            (nth i subst))
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
        ((trivia:guard ; comparisons with w and a number where that number is greater than w 
            (or (list 'eql (list 'w i) a) (list 'eql a (list 'w i)))
            (and (numberp a) (> a 9)))
            0)
        ((trivia:guard ; nested addition
            (or (list 'add (list 'add a rest) b) 
                (list 'add b (list 'add a rest)) 
                (list 'add b (list 'add rest a)) 
                (list 'add (list 'add rest a) b))
            (and (numberp a) (numberp b)))
            (list 'add (+ a b) rest))
        ((trivia:guard ; nested product
            (or (list 'mul (list 'mul a rest) b) 
                (list 'mul b (list 'mul a rest)) 
                (list 'mul b (list 'mul rest a)) 
                (list 'mul (list 'mul rest a) b))
            (and (numberp a) (numberp b)))
            (list 'mul (* a b) rest))
        ;; ((or (list 'mul (list 'add b c) a) (list 'mul a (list 'add b c))) ; a (b + c) -> ab + ac
        ;;     (list 'add (list 'mul a b) (list 'mul a c)))
        ;; ((or (list 'mul (list 'eql b c) a) (list 'mul a (list 'eql b c))) ; mul a (b = c) -> ab = ac
        ;;     (list 'eql (list 'mul a b) (list 'mul a c)))
        ;; ((list 'mod (list 'add a b) c) ; (a + b) % c -> ((a % c) + (b % c)) % c
        ;;     (list 'mod (list 'add (list 'mod a c) (list 'mod b c)) c))
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
    (print (list i (simplify    
                *cache*
                (list 
                    8 ; 1   
                    1 ; 2   
                    5 ; 3   
                    1 ; 4   
                    4 ; 5   
                    1 ; 6   
                    7 ; 7   
                    1 ; 8   
                    1 ; 9   
                    6 ; 10  
                    1 ; 11  
                    3 ; 12  
                    8 ; 13  
                    1 ; 14  
                    );; *subst*
                *data*))))

; Solution A (largest): 92969593497992
; Solution B (smallest): 81514171161381
