(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")
(require "cl-ppcre")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t))
                (line-ls (mapcar (lambda (line) (coerce line 'list)) lines)))
        (mapcar #'parse-line line-ls)))

(defun find-comma-i (line-l)
    (let ((open-brackets 0))
        (loop for i in (alexandria:iota (- (length line-l) 1) :start 1) do
            (let ()
                (if (equal (nth i line-l) #\[)
                    (setq open-brackets (+ open-brackets 1))
                    nil)
                (if (equal (nth i line-l) #\])
                    (setq open-brackets (- open-brackets 1))
                    nil)
                (if (= 0 open-brackets)
                    (if (equal (nth i line-l) #\,)
                        (return i)
                        nil))))))

(defun parse-line (line-l)
    (cond   ((= (length line-l) 0) nil)
            ((equal (car line-l) #\[)
                (let ((i (find-comma-i line-l)))
                    (list 
                        (parse-line (subseq line-l 1 i))
                        (parse-line (subseq line-l (+ i 1) (- (length line-l) 1))))))
            (t (digit-char-p (car line-l)))))

(print (load-data))
