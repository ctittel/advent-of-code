(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")
(require "trivia")

(defun matcher (x) 
    (trivia:match x
        ((list 3 4) "found")
        ((list* _) x)))

(defun process (x)
    (if (listp x)
        (matcher (mapcar #'process x))
        x))

(defparameter l (list 1 2 3 4 (list 1 2 (list 3 4) 3 4)))
(print l)

;; (print (matcher l))
(print (process l))
