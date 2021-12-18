(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun x (xy) (nth 0 xy))
(defun y (xy) (nth 1 xy))

(defun in-target (xy)
    (and
        (>= (x xy) 241)
        (<= (x xy) 273)
        (>= (y xy) -97)
        (<= (y xy) -63)))

;; (defun decrease-towards-0 (x)
;;     (cond   ((> x 0) (- x 1))
;;             ((< x 0) (+ x 1))
;;             (t 0)))

;; (defun forward-vels (vels)
;;     (list (decrease-towards-0 (x vels)) (- (y vels) 1)))

;; (defun forward-xy (xy vels)
;;     (list (+ (x xy) (x vels)) (+ (y xy) (y vels))))

; (defun will-hit (xy vels)
;     (cond   ((> (x xy) 273) 'x-to-big)

(defun f (n)
    (/ (* n (- n 1)) 2))

(defun find-n (start lower upper)
    (let ((x (f start)))
        (cond   ((< x lower) (find-n (+ start 1) lower upper))
                ((> x upper) nil)
                (t start))))

(defun find-y (start-vy)
    (let*  ((h (f start-vy))
            (nd (find-n 1 (+ h 63) (+ h 97))))
        (if nd h (find-y (- start-vy 1)))))

(print (find-n 1 241 273))
(print (find-y 1000))

; analytical solution for 1:
; - we want to stand still when we reached the target horizontally, so sum vx...1 must be between 241...273 - only true for vx0 = 23
; - we will be at y=0 again when going downwards, no matter the initial vy chosen; the fastest allowed vy at y = 0 to still hit the target is vy = 97; 
;   a vy=97 means a height of 4656

(defun possibilities-for-vx23 ()
    (+ 1 (- 97 63)))

(defun y-will-hit (n-steps startvy starty)
    (if (= 0 n-steps)
        (and (>= starty -97) (<= starty -63))
        (y-will-hit (- n-steps 1) (- startvy 1) (+ starty startvy))))

(defun x-will-hit (startvx startx n)
    (cond   ((and (> startvx 0) (< startx 241)) (x-will-hit (- startvx 1) (+ startx startvx) (+ n 1)))
            ((and (>= startx 241) (<= startx 273)) n)
            (t nil)))

(defun count-vy-possibilities (n-steps)
    (length
        (remove-if #'null
            (loop for vy in (alexandria:iota 98) collect
                (y-will-hit n-steps vy 0)))))

(defun problem2 ()
    (+ (possibilities-for-vx23)
        (apply #'+
            (remove-if #'null 
                (loop for vx in (alexandria:iota 49) collect
                    (let ((n (x-will-hit vx 0 0)))
                        (if n (count-vy-possibilities n) nil)))))))



;; (print (count-vy-possibilities 23))
;; (print (problem2)) ; wrong: 45 43