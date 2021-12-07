(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data () 
    (mapcar #'parse-integer (split-sequence:split-sequence #\, 
        (car (split-sequence:split-sequence #\Newline 
            (alexandria:read-file-into-string "input.txt"))))))

; --- Done loading ---

(defun average (list)
    (/ (apply #'+ list) (length list)))

(defun diffs (goal positions)
    (apply #'+ (mapcar (lambda (x) (abs (- goal x))) positions)))

(defun calc-fuel-cost (dist) (/ (* dist (+ 1 dist)) 2))

(defun diffs2 (goal positions)
    (apply #'+ (mapcar (lambda (x) (calc-fuel-cost (abs (- goal x)))) positions)))

(defun brute-force (positions)
    (let* ( (max-pos (apply #'max positions))
            (min-pos (apply #'min positions)))
        (apply #'min 
            (mapcar 
                (lambda (mid) (diffs mid positions)) 
                (alexandria:iota (- max-pos min-pos) :start min-pos)))))

(defun problem2 (positions)
    (let* ( (max-pos (apply #'max positions))
            (min-pos (apply #'min positions)))
        (apply #'min 
            (mapcar 
                (lambda (mid) (diffs2 mid positions)) 
                (alexandria:iota (- max-pos min-pos) :start min-pos)))))

(print (brute-force (load-data)))
(print (problem2 (load-data)))
