(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-grid() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
        (mapcar (lambda (line) (mapcar #'digit-char-p (coerce line 'list))) 
                lines)))

; ---

(defun get-cell-itm (grid x y)
    (nth x (nth y grid)))

(defun update-cell (grid x y)
    (setf (nth x (nth y grid))
        (+
            (get-cell-itm grid x y)
            (cond   ((and (= 0 x) (= 0 y)) 0)
                    ((= 0 x) (get-cell-itm grid x (- y 1)))
                    ((= 0 y) (get-cell-itm grid (- x 1) y))
                    (t (min (get-cell-itm grid x (- y 1))
                            (get-cell-itm grid (- x 1) y)))))))

(defun calc-costs (grid)
    (loop for y in (alexandria:iota (length grid)) do
        (loop for x in (alexandria:iota (length (car grid))) do
            (update-cell grid x y))))

(defun problem1 (grid)
    (let* ((costgrid (calc-costs grid)))
        (- (car (last (car (last grid)))) 
            (car (car grid)))))

(print (problem1 (load-grid))) ; too high: 147665780682923711971804561743820527253137345314183522211703
                                ; too low: 274
                                ; too low: 395
                                ; 402 false
                                ; 404 false