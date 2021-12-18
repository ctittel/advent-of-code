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

(defun parse-line-tree (line-l)
    (cond   ((= (length line-l) 0) nil)
            ((equal (car line-l) #\[)
                (let ((i (find-comma-i line-l)))
                    (list 
                        (parse-line (subseq line-l 1 i))
                        (parse-line (subseq line-l (+ i 1) (- (length line-l) 1))))))
            (t (digit-char-p (car line-l)))))

(defun parse-line-tree (line-l)


(print (load-data))

;; (defun add-left (l num)
;;     (reverse (add-rigth (reverse l) num)))

;; (defun add-right (l num)
;;     (let ()
;;         (loop for i in (alexandria:iota (length l)) do
;;             (if (numberp (nth i l))
;;                 (let ()
;;                     (incf (nth i l) num)
;;                     (return l))
;;                 nil))
;;         l))

;; (defun tryexplode (l)
;;     (let* ((open[ 0))
;;         (loop for i in (alexandria:iota (length l)) do
;;             (let ()
;;                 (if (equal (nth i l) #\[)
;;                     (incf open[)
;;                     (if (equal (nth i l) #\])
;;                         (decf open[)
;;                         nil))
;;                 (if (>= open[ 4)
;;                     (return
;;                         (append
;;                             (add-left (subseq l 0 i) (nth (+ i 1) l))
;;                             (list 0)
;;                             (add-right (subseq l (+ i 4)) (nth (+ i 2) l)))))))))

;; (defun trysplit (l)
;;     (let ()
;;         (loop for i in (alexandria:iota (length l)) do
;;             (let ((x (nth i l)))
;;                 (if (and (numberp x) (>= x 10))
;;                     (return
;;                         (append
;;                             (subseq l 0 i)
;;                             (list   #\[
;;                                     (floor (/ x 2))
;;                                     (ceiling (/ x 2))
;;                                     #\])
;;                             (subseq l (+ i 1)))))))
;;         nil))
