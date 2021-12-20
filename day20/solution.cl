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
    (binlist-to-num-aux (reverse binlist) 0))

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

; append 0s to all sides
(defun buffer-img (n img)
    (print (list "buffer-img" img))
    (if (= n 0)
        img
        (buffer-img
            (- n 1)
            (mapcar
                (lambda (line) (append (list 0) line (list 0)))
                (append
                    (list (make-list (length (car img)) :initial-element 0))
                    img
                    (list (make-list (length (car img)) :initial-element 0)))))))

(defun iota (start stop)
    (alexandria:iota (- stop start) :start start))

(defun step-img (img)
    (let* ((buffered (buffer-img 3 img)))
        (loop for y in (iota 1 (- (length buffered) 1)) collect
            (loop for x in (iota 1 (- (length (car buffered)) 1)) collect
                (calc-next-val (list x y) buffered)))))

(defun steps-img (n img)
    (if (= 0 n)
        img
        (steps-img (- n 1) (step-img img))))

;; (print (load-img))
(defun problem1 ()
    (apply #'+ 
        (mapcar
            (lambda (line) (apply #'+ line)) 
            (steps-img 2 (load-img)))))

;; (print (problem1))

(defparameter testimg (list (list 1 1 1)))
;; (loop for i in (alexandria:iota 5) do
;;     (setq testimg (step-img testimg))
;;     (print testimg))

(print (window-step (list 0 0 0 0 0 0 0 0 0)))
(print *algo*)

; A wrong: 4168 (too low)
