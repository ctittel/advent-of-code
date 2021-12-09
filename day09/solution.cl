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

(defun get-window (grid xy)
  (remove-if-not
      (lambda (xyw) (valid-coord grid xyw)) 
      (list
        (list (x xy) (- (y xy) 1))
        (list (- (x xy) 1) (y xy))
        (list (+ 1 (x xy)) (y xy))
        (list (x xy) (+ 1 (y xy))))))

(defun get-parent (grid xy)
  (let* ((val (get-val grid xy))
          (window (get-window grid xy))
          (xymin (reduce (lambda (current next)
                            (if (< (get-val grid next) (get-val grid current)) next current)) 
                    window       
                    :initial-value (nth 0 window))))
    (cond 
        ((>= val 9) 'border)
        ((> val (get-val grid xymin)) xymin)
        ((< val (get-val grid xymin)) 'root)
        (t 'fail))))

(defun build-tree (grid)
  (let* ((*tree* (make-hash-table :test 'equal)))
    (loop for x in (alexandria:iota (width-grid grid)) do
      (loop for y in (alexandria:iota (height-grid grid)) do
        (let* ( (xy (list x y))
                (parent (get-parent grid xy)))
              (setf (gethash parent *tree*) (append (gethash parent *tree*) (list xy))))))
  *tree*))    

(defun count-childs (*tree* xy)
  (+ 1
      (apply #'+ (mapcar (lambda (xxyy) (count-childs *tree* xxyy)) (gethash xy *tree*)))))

(defun problem2 (grid)
  (let* ( (tree (build-tree grid))
          (basins (mapcar (lambda (r) (count-childs tree r)) (gethash 'root tree))))
    (apply #'* (subseq (sort basins #'>) 0 3))))

(print (length (gethash 'root (build-tree (load-grid)))))
(print (problem2 (load-grid)))
