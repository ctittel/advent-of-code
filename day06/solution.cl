(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data () 
    (mapcar #'parse-integer (split-sequence:split-sequence #\, 
        (car (split-sequence:split-sequence #\Newline 
            (alexandria:read-file-into-string "input.txt"))))))

; ;; --- Below the actual problem ---

(defun forward-day (freq-by-age)
    (let*  (   (current (car freq-by-age))
                (new-freqs (append (cdr freq-by-age) '(0))))
        (setf (nth 6 new-freqs) (+ current (nth 6 new-freqs)))
        (setf (nth 8 new-freqs) (+ current (nth 8 new-freqs)))
        new-freqs
))

(defun count-freqs (data)
    (mapcar (lambda (x) (count x data)) (alexandria:iota 10)))

(defun forward-days (initial-freq n-days)
    (reduce 
        (lambda (current-freq n-day) (forward-day current-freq))
        (alexandria:iota n-days) :initial-value initial-freq))

(defun sum-list (l) (reduce #'+ l))

(print (sum-list (forward-days (count-freqs (load-data)) 80)))
(print (sum-list (forward-days (count-freqs (load-data)) 256)))
