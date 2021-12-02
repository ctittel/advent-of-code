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

(defun reduce-fn (current-sum next-item)
    (append current-sum (list (+ (car (last current-sum)) next-item))))

; lists: ((horizontal1 vertical1) (horizontal2 vertical2) ...)
(defun problem2 (lists)
    (let* ((unpacked-lists (apply #'mapcar #'list lists))
        (horizontals (car unpacked-lists))
        (verticals (car (cdr unpacked-lists)))
        (aims (reduce #'reduce-fn verticals :initial-value '(0)))
        )
            (* (reduce #'+ horizontals)
                (reduce #'+ (mapcar #'* horizontals aims))
            )
    )
)

(print (problem1 (load-data)))
(print (problem2 (load-data)))
