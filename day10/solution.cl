(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-lines () 
        (split-sequence:split-sequence 
          #\Newline 
          (alexandria:read-file-into-string "input.txt") 
          :remove-empty-subseqs t))

; --- Done loading ---

(defparameter brackets-opening (list #\( #\[ #\{  #\< ))
(defparameter brackets-closing (list #\) #\] #\}  #\> ))
(defparameter brackets-points  (list 3   57  1197 25137))
(defparameter autocomplete-points  (list 1  2  3  4))

(defun process-line (line required-closing-brackets)
  (let* ((c (car line))
        (poso (position c brackets-opening)))
    (cond ((eq c nil) (if required-closing-brackets (list 'incomplete required-closing-brackets) (list 'correct)))
          (poso (process-line   (cdr line) 
                                (append (list (nth poso brackets-closing)) required-closing-brackets)))
          ((eq c (car required-closing-brackets)) (process-line (cdr line) (cdr required-closing-brackets)))
          (t (list 'illegal c)))))

(defun get-mistakes (lines)
  (mapcar (lambda (line) (process-line (coerce line 'list) (list))) lines))

(defun problem1 (lines)
  (let* ((mistakes (get-mistakes lines))
          (illegals (remove-if-not (lambda (x) (eq (car x) 'illegal)) mistakes))
          (illegal-points (mapcar (lambda (x) (nth (position (nth 1 x) brackets-closing) brackets-points)) illegals)))
    (apply #'+ illegal-points)))

(defun score-missing (missing-brackets)
  (reduce 
    (lambda (total c) (+ (* 5 total) (nth (position c brackets-closing) autocomplete-points)))
    missing-brackets
    :initial-value 0))

(defun problem2 (lines)
  (let* ((mistakes (get-mistakes lines))
          (incompletes (remove-if-not (lambda (x) (eq (car x) 'incomplete)) mistakes))
          (missing-brackets (mapcar (lambda (x) (nth 1 x)) incompletes))
          (incomplete-points (mapcar #'score-missing missing-brackets)))
    (nth (/ (- (length incomplete-points) 1) 2 ) (sort incomplete-points #'>))))

(print (problem1 (load-lines)))
(print (problem2 (load-lines)))
