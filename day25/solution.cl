(load "~/quicklisp/setup.lisp")
(require "alexandria")
(require "split-sequence")

;; ----- PARSING

(defun load-data() 
        (let* ( (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t)))
            (mapcar
                (lambda (line) (mapcar #'parse-char (coerce line 'list)))
                lines)))

(defun parse-char (c)
    (cond 
        ((equal c #\. ) nil)
        ((equal c #\v ) 'down)
        ((equal c #\> ) 'right)))

(defun step-row (row agent)
    (let (  (prev-l (append (last row) (butlast row)))
            (next-l (append (cdr row) (list (car row)))))
        (mapcar
            (lambda (prev curr next) 
                (cond 
                    ((and (null curr) (equal agent prev)) agent)
                    ((and (equal agent curr) (null next)) nil)
                    (t curr)))
            prev-l
            row
            next-l)))

(defun turn-clockwise (table)
    (apply
        #'mapcar
        #'list
        (reverse table)))

(defun turn-counterclockwise (table)
    (turn-clockwise (turn-clockwise (turn-clockwise table))))

(defun step-table (table)
    (let* ( (table-r (mapcar (lambda (row) (step-row row 'right)) table)))
        (turn-clockwise
            (mapcar 
                (lambda (row) (step-row row 'down)) 
                (turn-counterclockwise table-r)))))

(defun problem1 (data i)
    ;; (print i)
    (let* ((ndata (step-table data)))
        (if (equal data ndata)
            i
            (problem1 ndata (+ i 1)))))

;; (print (step-table (load-data)))
(print (problem1 (load-data) 1))

;; (defparameter *table* (list (list 10 11 12) (list 20 21 22) (list 30 31 32)))
;; (print *table*)
;; (print (turn-clockwise *table*))
