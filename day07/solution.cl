(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data () 
    (mapcar #'parse-integer (split-sequence:split-sequence #\, 
        (car (split-sequence:split-sequence #\Newline 
            (alexandria:read-file-into-string "input.txt"))))))

; --- Done loading ---

(defun costA (goal positions)
    (apply #'+ (mapcar (lambda (x) (abs (- goal x))) positions)))

(defun sum-to-n (n) (/ (* n (+ 1 n)) 2))

(defun costB (goal positions)
    (apply #'+ (mapcar (lambda (x) (sum-to-n (abs (- goal x)))) positions)))

(defun find-min (cost-fn positions)
    (let* ( (max-pos (apply #'max positions))
            (min-pos (apply #'min positions)))
        (apply #'min 
            (mapcar 
                (lambda (mid) (funcall cost-fn mid positions)) 
                (alexandria:iota (- max-pos min-pos) :start min-pos)))))

(print (find-min #'costA (load-data)))
(print (find-min #'costB (load-data)))
