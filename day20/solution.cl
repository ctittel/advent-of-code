(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (mapcar #'parse-line (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t))))
            lines))

(defun parse-line (line)
    (mapcar
        (lambda (c) 
            (cond   ((equal c #\.) 0)
                    ((equal c #\#) 1)
                    (t "UNKNOWN CHAR")))
        (coerce line 'list)))

(defun load-algo () (car (load-data)))
(defun load-img () (cdr (load-data)))

;; --- Parsing input done ---

(defparameter *algo* (load-algo))

(defun binlist-to-num (binlist)
    (binlist-to-num-aux binlist 0))

(defun binlist-to-num-aux (binlist current)
    (if binlist
        (binlist-to-num-aux (cdr binlist)
                        (+ (* current 2) (car binlist)))
        current))

(defun window-step (window)
    (nth (binlist-to-num window) *algo*))

(defun x (xy) (nth 0 xy))
(defun y (xy) (nth 1 xy))

(defun get-window-coords (xy)
    (apply #'append
        (loop for yy in (list -1 0 1) collect
            (loop for xx in (list -1 0 1) collect
                (list
                    (+ (x xy) xx)
                    (+ (y xy) yy))))))

(defun get-val (xy img)
    (nth (x xy) 
        (nth (y xy) img)))

(defun calc-next-val (xy img)
    (window-step (mapcar
                    (lambda (z) (get-val z img))
                    (get-window-coords xy))))

(defun add-buffers (n symbol img)
    (if (= n 0)
        img
        (add-buffers
            (- n 1)
            symbol
            (mapcar
                (lambda (line) (append (list symbol) line (list symbol)))
                (append
                    (list (make-list (length (car img)) :initial-element symbol))
                    img
                    (list (make-list (length (car img)) :initial-element symbol)))))))

(defun remove-buffers (n img)
    (if (= n 0)
        img
        (remove-buffers 
            (- n 1)
            (cdr (butlast
                (mapcar
                    (lambda (line) (cdr (butlast line)))
                    img))))))

(defun iota (start stop)
    (alexandria:iota (- stop start) :start start))

(defun step-img (img)
    (loop for y in (iota 1 (- (length img) 1)) collect
        (loop for x in (iota 1 (- (length (car img)) 1)) collect
            (calc-next-val (list x y) img))))

(defun steps-img (n img)
    (if (= 0 n)
        img
        (steps-img 
            (- n 1)
            (let ((s (rem n 2)))
                (remove-buffers 
                    1
                    (step-img (add-buffers 3 s img)))))))

(defun problem1 ()
    (apply #'+ 
        (mapcar
            (lambda (line) (apply #'+ line)) 
            (steps-img 2 (load-img)))))

(defun problem2 ()
    (apply #'+ 
        (mapcar
            (lambda (line) (apply #'+ line)) 
            (steps-img 50 (load-img)))))

(print (problem1))
(print (problem2))

(defparameter testimg (list (list 1 1 1)))

; A wrong: 4168 (too low), 4520 (too low); 4928 (too high); correct: 4917

