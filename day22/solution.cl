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

(defun calc-intersection-area (a b)
    (let* ((deltas  (mapcar
                        (lambda (xa xb)
                            (-  (min (second xa) (second xb))
                                (max (first xa) (first xb))))
                        a
                        b)))
        (if (some (lambda (delta) (< delta 0)) deltas)
            0
            (apply #'* deltas))))

(defun intersect (box other)
    (let* ((axes  (mapcar
                        (lambda (xbox xother)
                            (list   (first xbox)
                                    (max (first xa) (first xb)))
                                    (min (second xa) (second xb))
                        box
                        other)))
        (if (some (lambda (delta) (< delta 0)) deltas)
            0
            (apply #'* deltas))))

; (print (load-data))
; (print (calc-intersection-area
;             (list (list -10 1) (list -10 1) (list -10 1))
;             (list (list -10 1) (list -10 1) (list -10 0))))

(defun intersect )
