(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defparameter letters (list #\N #\O #\P #\V #\C #\B #\F #\H #\S #\K))

(defun load-data()
    (let* (  
            (lines (split-sequence:split-sequence 
                        #\Newline 
                        (alexandria:read-file-into-string "input.txt")
                        :remove-empty-subseqs t))
            (input-line (car lines))
            (rule-lines (mapcar 'parse-rule (subseq lines 1)))
        )
        (list input-line rule-lines)))

(defun parse-rule (rule)
    (let ((rulestr (coerce rule 'list)))
  (list (list (nth 0 rulestr) (nth 1 rulestr)) (nth 6 rulestr))))

(defun hashtable-from-list (l)
    (let ((table (make-hash-table :test 'equal)))
        (loop for entry in l do 
            (setf (gethash (car entry) table) (nth 1 entry)))
        table))

(defun count-down (n pair rules-map cache)
    (let ((key (list n pair)))
        (if (null (gethash key cache))
            (setf (gethash key cache) (count-down-aux n pair rules-map cache))
            nil)
        (gethash key cache)))

(defun count-down-aux (n pair rules-map cache)
    (cond 
        ((= 1 n) 
            (let ((l (list (nth 0 pair) (gethash pair rules-map) (gethash pair rules-map) (nth 1 pair))))
                (mapcar (lambda (letter) (/ (count letter l) 2)) letters)))
        (t 
            (mapcar #'+ 
                    (count-down (- n 1) (list (car pair) (gethash pair rules-map)) rules-map cache)
                    (count-down (- n 1) (list (gethash pair rules-map) (nth 1 pair)) rules-map cache)))))

(defun problemx (n input-l rules-map)
    (let* ( (cache (make-hash-table :test 'equal))
            (counts (mapcar  (lambda (i1 i2) (count-down n (list i1 i2) rules-map cache)) 
                            (butlast input-l) (cdr input-l)))
            (total-counts (apply #'mapcar #'+ counts))
            (pos1 (position (car input-l) letters))
            (pos2 (position (car (last input-l)) letters))                
            )
        (setf (nth pos1 total-counts) (+ (nth pos1 total-counts) (/ 1 2)))
        (setf (nth pos2 total-counts) (+ (nth pos2 total-counts) (/ 1 2)))
        (- (apply #'max total-counts) (apply #'min total-counts))))

(let* ( (loaded (load-data))
        (input-l (coerce (car loaded) 'list))
        (rules (nth 1 loaded))
        (rules-map (hashtable-from-list rules)))
    (print (problemx 10 input-l rules-map))
    (print (problemx 40 input-l rules-map))
)
