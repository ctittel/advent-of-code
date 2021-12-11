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

(defun x (xy) (nth 0 xy))
(defun y (xy) (nth 1 xy))

(defun valid-coord (grid xy)
  (and (>= (x xy) 0) (>= (y xy) 0) (< (x xy) (width-grid grid)) (< (y xy) (height-grid grid))))

(defun get-val (grid xy) (nth (x xy) (nth (y xy) grid)))
(defun set-val (grid xy new-val) (setf (nth (x xy) (nth (y xy) grid)) new-val))

(defun get-all-coords (llxy urxy)
    (apply #'append
    (loop for x in (alexandria:iota (+ 1 (- (x urxy) (x llxy))) :start (x llxy)) collect
        (loop for y in (alexandria:iota (+ 1 (- (y urxy) (y llxy))) :start (y llxy)) collect (list x y)))))

(defun get-window (grid xy)
    (remove-if-not
        (lambda (xyw) (and (valid-coord grid xyw) (not (equal xy xyw))))
        (get-all-coords 
            (list (- (x xy) 1) (- (y xy) 1))
            (list (+ 1 (x xy)) (+ 1 (y xy))))))

(defun increment (grid)
    (mapcar (lambda (line)
                (mapcar (lambda (v) (+ v 1)) line))
            grid))

(defun explode (grid)
    (let* ((to-explode (remove-if-not 
                        (lambda (xy) (> (get-val grid xy) 9)) 
                        (get-all-coords (list 0 0) (list (- (width-grid grid) 1) (- (height-grid grid) 1))))))
        (if (> (length to-explode) 0)
            (let () (loop for xy in to-explode do 
                        (let () (loop for neighbor in (get-window grid xy) do
                                    (if (> (get-val grid neighbor) 0) 
                                        (set-val grid neighbor (+ (get-val grid neighbor) 1)) 
                                        ()))
                                (set-val grid xy 0)))
                    (explode grid))
            grid)))

(defun count-flat (itm grid)
    (count itm (apply #'append grid)))


(defun problem1 (nsteps grid count)
    (if (= 0 nsteps)
        count
        (let ((new-grid (explode (increment grid))))
            (problem1 (- nsteps 1) new-grid (+ count (count-flat 0 new-grid))))))

(print (problem1 100 (load-grid) 0))    ; 1673
