(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(require "split-sequence")

(defun load-lines ()
  (with-open-file (stream "input.txt")
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-line-of-numbers (line separator)
    (mapcar #'parse-integer (split-sequence:split-sequence separator line :remove-empty-subseqs t)))

(defun load-numbers ()
    (parse-line-of-numbers (nth 0 (load-lines)) #\, ))

(defun parse-board (lines)
    (mapcar (lambda (line) (parse-line-of-numbers line #\SPACE )) lines))

(defun load-boards ()
    (let* ( (data (subseq (load-lines) 2))     
            (number-of-boards (/ (length data) 6 ))
            (start-idxs (loop :for n :below number-of-boards :collect n))
        )
        (mapcar (lambda (i-board) 
                    (parse-board (subseq data (* 6 i-board) (+ 5 (* 6 i-board)) )))
                start-idxs)        
))

;; ---- Problem solving ----

(defun get-rows (board) board)

(defun get-columns (board) (apply #'mapcar #'list board))

(defun check-number (board new-drawn-number)
    (mapcar 
        (lambda (row) (substitute nil new-drawn-number row)) 
        board)
)

(defun only-nil (row) (every 'null row))

(defun is-winner-board (board)
    (if (or
            (some #'only-nil (get-rows board))
            (some #'only-nil (get-columns board)))
        board nil
))

(defun sum-board (board)
    (let ((flat-board (reduce #'append board :initial-value '())))
        (apply #'+ (remove-if #'null flat-board))
))

(defun play-boards (boards new-drawn-number)
    (mapcar (lambda (board) (check-number board new-drawn-number)) boards))

(defun problem1 (boards drawn-numbers i)
    (let* ( (current-number (car drawn-numbers))
            (forwarded-boards (play-boards boards current-number))
            (winner (some #'is-winner-board forwarded-boards)))
        (if winner 
            (* current-number (sum-board winner))
            (problem1 forwarded-boards (cdr drawn-numbers) (+ 1 i)))
))

(defun problem2 (boards drawn-numbers i)
    (let* ( (current-number (car drawn-numbers))
            (forwarded-boards (play-boards boards current-number))
            ; (winner (some #'is-winner-board forwarded-boards))
            (no-winners (remove-if #'is-winner-board forwarded-boards)))
        (if (= 0 (length no-winners))
            (* current-number (sum-board (car forwarded-boards)))
            (problem2 no-winners (cdr drawn-numbers) (+ 1 i)))
))

(print (problem1 (load-boards) (load-numbers) 0))
(print (problem2 (load-boards) (load-numbers) 0))
