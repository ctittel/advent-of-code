(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(require "split-sequence")

(defun load-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-item line))))

(defun load-data () (load-file "input.txt"))

(defun parse-item (item)
    (apply #'parse-one-item
        (split-sequence:split-sequence #\  item)))


(defun parse-one-item (command thenumber)
        (let ((num (parse-integer thenumber)))
            (cond ((equalp command "forward") (list num 0))
                    ((equalp command "up") (list 0 (- num)))
                    ((equalp command "down") (list 0 num))
                    (t "fail")
                    )))

(defun problem1 (lists)
    (apply #'* (apply #'mapcar #'+ lists))
)

(defun state-horizontal (state)
    (first state))

(defun state-vert (state)
    (first (cdr state)))

(defun state-aim (state)
    (car (last state)))

(defun action-forward (action) (first action))
(defun action-downward (action) (first (cdr action)))

; state: (horizontal vertical aim)
(defun next-state (prev-state action)
    (list
        (+ (state-horizontal prev-state) (action-forward action))
        (+ (state-vert prev-state) (* (state-aim prev-state) (action-forward action)))
        (+ (state-aim prev-state) (action-downward action))))

(defun forward-state (lists)
    (reduce #'next-state lists :initial-value '(0 0 0))
)

(defun problem2 (data)
    (let ((final-state (forward-state data)))
        (* (first final-state) (first (cdr final-state))))
)

(print (problem1 (load-data)))
(print (problem2 (load-data)))
