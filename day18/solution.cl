(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")
(require "cl-ppcre")
(require "trivia")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "test.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t))
                (line-ls (mapcar (lambda (line) (coerce line 'list)) lines)))
        (mapcar #'parse-line-tree line-ls)))

(defun find-comma-i (line-l)
    (let ((open-brackets 0))
        (loop for i in (alexandria:iota (- (length line-l) 1) :start 1) do
            (let ()
                (if (equal (nth i line-l) #\[)
                    (setq open-brackets (+ open-brackets 1))
                    nil)
                (if (equal (nth i line-l) #\])
                    (setq open-brackets (- open-brackets 1))
                    nil)
                (if (= 0 open-brackets)
                    (if (equal (nth i line-l) #\,)
                        (return i)
                        nil))))))

(defun parse-line-tree (line-l)
    (cond   ((= (length line-l) 0) nil)
            ((equal (car line-l) #\[)
                (let ((i (find-comma-i line-l)))
                    (list 
                        (parse-line-tree (subseq line-l 1 i))
                        (parse-line-tree (subseq line-l (+ i 1) (- (length line-l) 1))))))
            (t (digit-char-p (car line-l)))))

;; (defun parse-line-tree (line-l)


;; (defun add-left (l num)
;;     (reverse (add-rigth (reverse l) num)))

;; (defun add-right (l num)
;;     (let ()
;;         (loop for i in (alexandria:iota (length l)) do
;;             (if (numberp (nth i l))
;;                 (let ()
;;                     (incf (nth i l) num)
;;                     (return l))
;;                 nil))
;;         l))

;; (defun tryexplode (l)
;;     (let* ((open[ 0))
;;         (loop for i in (alexandria:iota (length l)) do
;;             (let ()
;;                 (if (equal (nth i l) #\[)
;;                     (incf open[)
;;                     (if (equal (nth i l) #\])
;;                         (decf open[)
;;                         nil))
;;                 (if (>= open[ 4)
;;                     (return
;;                         (append
;;                             (add-left (subseq l 0 i) (nth (+ i 1) l))
;;                             (list 0)
;;                             (add-right (subseq l (+ i 4)) (nth (+ i 2) l)))))))))

;; (defun trysplit (l)
;;     (let ()
;;         (loop for i in (alexandria:iota (length l)) do
;;             (let ((x (nth i l)))
;;                 (if (and (numberp x) (>= x 10))
;;                     (return
;;                         (append
;;                             (subseq l 0 i)
;;                             (list   #\[
;;                                     (floor (/ x 2))
;;                                     (ceiling (/ x 2))
;;                                     #\])
;;                             (subseq l (+ i 1)))))))
;;         nil))



;; (defun explode4 (tree)
;;     (trivia:match tree
;;         (
;;             (list (list l r) rr) a) b) c)
;;             (list (list (list (list 0 (+ rr r))    a) b) c)
;;         )
;;         (
;;             (list (list (list (list ll (list l r)) rr)  b) c)
;;             (list (list (list (list (+ ll l) (+ rr r))  b) c)
;;         )
;; ))

;; (defun tryexplode (tree depth)
;;     (if (>= depth 4)
;;         (list (first tree) 0 (second tree))
;;         (trivia:match tree

        
;;         )
;; )

(defun flatten-tree (tree)
    (if (listp tree)
        (append
            (list #\[)
            (flatten-tree (first tree))
            (flatten-tree (second tree))
            (list #\]))
        (list tree)))

(defun add-left (l num)
    (reverse (add-right (reverse l) num)))

(defun add-right (l num)
    (let ()
        (loop for i in (alexandria:iota (length l)) do
            (if (numberp (nth i l))
                (let ()
                    (incf (nth i l) num)
                    (return l))
                nil))
        l))

(defun explode (tree-l)
    ;; (print (list "explode" tree-l))
    (let ((brackets 0))
        (loop for i in (alexandria:iota (length tree-l)) do
            (let ((curr (nth i tree-l)))
                ;; (print (list "curr=" curr))
                (if (equal curr #\[ ) (incf brackets) nil)
                (if (equal curr #\] ) (decf brackets) nil)
                (if (>= brackets 5)
                    (return 
                        (append
                            (add-left (subseq tree-l 0 i) (nth (+ i 1) tree-l))
                            (list 0)
                            (add-right (subseq tree-l (+ i 4)) (nth (+ i 2) tree-l))))
                    nil)))))

(defun splitt (tree-l)
    ;; (print (list "splitt" tree-l))
    (loop for i in (alexandria:iota (length tree-l)) do
        (let* (  (curr (nth i tree-l)))
            (if (and (numberp curr) (>= curr 10))
                (return
                    (append
                        (subseq tree-l 0 i)
                        (list #\[ (floor (/ curr 2)) (ceiling (/ curr 2)) #\])
                        (subseq tree-l (+ i 1))))
                nil)))
    nil)

(defun simplify (tree-l)
    ;; (print (list "simplify" tree-l))
    (let* ( (exploded (explode tree-l)))
        (if (null exploded)
            (let ((splitted (splitt tree-l)))
                (if (null splitted)
                    tree-l
                    (simplify splitted)))
            (simplify exploded))))

(defun magnitude (tree-l)
    (if (and (= (length tree-l) 1) (numberp (car tree-l)))
        (car tree-l)
        (loop for i in (alexandria:iota (length tree-l)) do
            (if (and    (equal (nth i tree-l) #\[)
                        (numberp (nth (+ i 1) tree-l))
                        (numberp (nth (+ i 2) tree-l))
                        (equal (nth (+ i 3) tree-l) #\]))
                (return
                    (magnitude
                        (append
                            (subseq tree-l 0 i)
                            (list
                                (+  (* (nth (+ i 1) tree-l) 3)
                                    (* (nth (+ i 2) tree-l) 2)))
                            (subseq tree-l (+ i 4)))))
                nil))))

(defun problem1 (tree-ls)
    (let ((total
            (reduce 
                (lambda (total x)
                    (simplify
                        (append
                            (list #\[ )
                            (simplify total)
                            (simplify x)
                            (list #\] )
                        )))
                (cdr tree-ls)
                :initial-value (car tree-ls))))
        (magnitude total)))

;; Too high: 9328

;; (loop for tree in (load-data) do
;;     (print (magnitude (simplify (flatten-tree tree)))))

(defun load-lists ()
    (mapcar 
        (lambda (tree) (simplify (flatten-tree tree)))
        (load-data)))

(print (problem1 (load-lists)))

;; (mapcar #'test (load-data))
