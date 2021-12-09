(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-grid() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
        (mapcar (lambda (line) (mapcar #'digit-char-p (coerce line 'list))) 
                lines)))

; --- Done loading ---

(defun height-grid (grid) (length grid))
(defun width-grid (grid) (length (car grid)))

(defun get-val (x y grid)
  (let* ((height (height-grid grid)) (width (width-grid grid)))
    (if (or (< x 0) (< y 0) (>= x width) (>= y height)) nil (nth x (nth y grid)))))

(defun get-window (x y grid)
  (list (get-val x (- y 1) grid) (get-val (- x 1) y grid) (get-val (+ x 1) y grid) (get-val x (+ y 1) grid)))

(defun check-local-minima (middle window)
  (< middle 
     (reduce (lambda (total x) (if (or (null x) (<= total x)) total x)) window :initial-value (+ 1 middle)))) 

(defun get-windows (grid)
  (mapcar 
    (lambda (yy) 
      (mapcar 
        (lambda (xx) (get-window xx yy grid)) 
        (alexandria:iota (width-grid grid))))
    (alexandria:iota (height-grid grid))))

(defun problem1 (grid)
  (let* ((wins (apply #'append (get-windows grid)))
         (middles (apply #'append grid))
         (minimas (remove nil (mapcar (lambda (mid win) (if (check-local-minima mid win) mid nil)) middles wins))))
     (+ (length minimas) (apply #'+ minimas))))

(print (problem1 (load-grid)))
