(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (mapcar #'parse-line (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t))))
            lines))

(defun parse-line (line)
    (let* ( (splitted (split-sequence:split-sequence #\  line :remove-empty-subseqs t))
            (onoff (first splitted))
            (coords (parse-coords (second splitted))))
            (if (equal onoff "on")
                (list t coords)
                (list nil coords))))

(defun remove-all (chars s)
    (reduce
        (lambda (total c) (remove c total))
        chars
        :initial-value s))

(defun parse-coords (coord-string)
    (let* (( coords (split-sequence:split-sequence #\, coord-string)))
    (mapcar
        (lambda (coord)
            (mapcar 
                (lambda (x) (parse-integer x))
                (split-sequence:split-sequence #\. (remove-all (list #\x #\y #\z #\=) coord) :remove-empty-subseqs t)))
        coords)))

;; --- Done Loading

; l is a list of lists of elements that may be at each spot
(defun get-all-combs (l)
    ;; (print l)
    (if (= (length l) 1)
        (mapcar #'list (car l))
        (apply #'append
            (loop for x in (car l) collect
                (loop for rest in (get-all-combs (cdr l)) collect
                    (append (list x) rest))))))

; Returns a list of boxes, created by removing box b from box a
(defun get-subboxes (a b)
    (let* ((points ; list of  ((points of interest x) (points of interest y) (points of interest z))
                (mapcar
                    (lambda (xa xb)
                        (remove-if #'null
                            (list
                                (first xa)
                                (if (and (> (first xb) (first xa)) (< (first xb) (second xa))) (first xb) nil)
                                (if (and (> (second xb) (first xa)) (< (second xb) (second xa))) (second xb) nil)
                                (second xa))))
                    a
                    b)))
        (get-all-combs (mapcar
                            (lambda (x) (mapcar #'list (butlast x) (cdr x)))
                            points))))

(defun remove-intersections (a b)
    (remove-if
        (lambda (box) (intersects box b))
        ;; (lambda (box) (equal box b))
        (get-subboxes a b)))

; whether or not a and b intersect
(defun intersects (a b)
    (every
        #'identity
        (mapcar
            (lambda (rangea rangeb)
                (not (or
                        (>= (first rangea) (second rangeb))
                        (<= (second rangea) (first rangeb)))))
            a b)))

(defun process-inputs (t-boxes inputs)
    (loop for (val box) in inputs do
        ;; (print (list "val" val "box" box "t-boxes" t-boxes))
        (setq t-boxes
            (append
                (apply 
                    #'append
                    (mapcar
                        (lambda (other)
                            ;; (if (intersects other box)
                                (remove-intersections other box))
                                ;; (list other)))
                        t-boxes))
                (if val 
                    (list box) 
                    nil))))
    t-boxes)

(defun calc-area (box)
    (apply #'*
        (mapcar
            (lambda (coords) (- (second coords) (first coords)))
            box)))

(defun sum-boxes (boxes)
    (apply 
        #'+
        (mapcar #'calc-area boxes)))

;; (print (remove-intersections (list (list 0 10) (list 0 10)) (list (list 1 2) (list -1 1))))

;; (setq testa (list (list 0 10) (list 0 10)))
;; (setq testb (list (list 0 10) (list -1 110)))
;; (print (remove-intersections testa testb))
;; (print (sum-boxes (remove-intersections testa testb)))

(defun prepare-data (data)
    (loop for (v box) in data collect
        (list v
            (mapcar
                (lambda (boxx) (list (first boxx) (+ 1 (second boxx))))
                box))))

(let* (  (data (prepare-data (load-data)))
        (initial-t-box (second (car data)))
        (inputs (cdr data)))
    (print (subseq inputs 0 19))
    (print (sum-boxes
                (process-inputs (list (second (car data))) (subseq inputs 0 19))))

)

;; A too low: 585247
