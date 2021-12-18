(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")
(require "cl-ppcre")
(require "trivia")

(print
(loop for i in (alexandria:iota 100) do
    (let ()
        (if (= i 40)
            (return i)
            nil)))
)

(cond ((= 1 2) (print "hi") (print "test"))
 ((= 1 1) (print "hidd") (print "test"))
)