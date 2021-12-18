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