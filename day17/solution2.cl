(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun x (xy) (nth 0 xy))
(defun y (xy) (nth 1 xy))

(defun step-xy (xy vels)
    (mapcar #'+ xy vels))

(defun step-vels (vels)
    (list 
        (if (> (x vels) 0) 
            (- (x vels) 1)
            0) 
        (- (y vels) 1)))

(defun will-hit (xy vels)
    (cond 
            ((or (> (x xy) 273) (< (y xy) -97)) nil)
            ((and (>= (x xy) 241) (<= (y xy) -63)) t)
            (t (will-hit (step-xy xy vels) (step-vels vels)))))

(defun gen-combs ()
 (apply #'append
    (loop for vx in (alexandria:iota 252 :start 22) collect
        (loop for vy in (alexandria:iota 371 :start -273) collect
            (list vx vy)))))

(defun problem2 ()
    (mapcar (lambda (xy) (will-hit (list 0 0) xy))
            (gen-combs)))

(print (length (remove-if #'null (problem2))))
;; (print (gen-combs))
