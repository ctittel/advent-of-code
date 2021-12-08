(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-lines() (split-sequence:split-sequence #\Newline 
                        (alexandria:read-file-into-string "input.txt") :remove-empty-subseqs t))
(defun load-operands ()     
        (mapcar (lambda (line) 
                  (split-sequence:split-sequence #\  (car (split-sequence:split-sequence #\| line)) :remove-empty-subseqs t)) 
                (load-lines)))

(defun load-results ()     
        (mapcar (lambda (line) 
                  (split-sequence:split-sequence #\  (nth 1 (split-sequence:split-sequence #\| line)) :remove-empty-subseqs t)) 
                (load-lines)))

; --- Done loading ---

(defun char-to-bin (c)
        (cond 
                ((eq c #\a) #b1)
                ((eq c #\b) #b10)
                ((eq c #\c) #b100)
                ((eq c #\d) #b1000)
                ((eq c #\e) #b10000)
                ((eq c #\f) #b100000)
                ((eq c #\g) #b1000000)
))

(defun word-to-bin (word) (apply #'logior (mapcar #'char-to-bin (coerce word 'list))))

(defun wordlist-to-binlist (wordlist) (mapcar #'word-to-bin wordlist))

; (defun bin-to-int (num)
;         (cond 
;                 ((= num (word-to-bin "abcefg")) 0)
;                 ((= num (word-to-bin "cf")) 1)
;                 ((= num (word-to-bin "acdeg")) 2)
;                 ((= num (word-to-bin "acdfg")) 3)
;                 ((= num (word-to-bin "bcdf")) 4)
;                 ((= num (word-to-bin "abdfg")) 5)
;                 ((= num (word-to-bin "abdefg")) 6)
;                 ((= num (word-to-bin "acf")) 7)
;                 ((= num (word-to-bin "abcdefg")) 8)
;                 ((= num (word-to-bin "abcdfg")) 9)
;                 (t "fail")
; ))

(defun is1 (word) (= 2 (length word)))
(defun is4 (word) (= 4 (length word)))
(defun is7 (word) (= 3 (length word)))
(defun is8 (word) (= 7 (length word)))
(defun is1478 (word) (or (is1 word) (is4 word) (is7 word) (is8 word)))

(defun count1478 (list-of-strs)
        (apply #'+ 
          (mapcar 
            (lambda (strs) (length (remove-if (lambda (word) (not (is1478 word))) strs))) 
            list-of-strs)))

(defun sum-bits-aux (num sum)
        (if (> num 0) (sum-bits-aux (ash num -1) (+ sum (logand num 1))) sum))

(defun count1bits (bin-num) (sum-bits-aux bin-num 0))

(defun n-set-bits (bin-nums n) (remove-if-not (lambda (x) (= n (count1bits x))) bin-nums))

(defun get-mappings (bin-nums)
        (let* (
                (x1 (car (n-set-bits bin-nums 2)))
                (x4 (car (n-set-bits bin-nums 4)))
                (x7 (car (n-set-bits bin-nums 3)))
                (x8 (car (n-set-bits bin-nums 7)))
                (adg (apply #'logand (n-set-bits bin-nums 5)))
                (abfg (apply #'logand (n-set-bits bin-nums 6)))
                (a (- x7 x1))
                (d (logand adg x4))
                (g (- adg a d))
                (e (- x8 x4 a g))
                (b (- x4 x1 d))
                (f (- abfg a b g))
                (c (- x1 f))
                (mappings (list a b c d e f g))
        )
        (print mappings)))

(defun apply-mapping (bin-num mapping)
        (let (
                (a (nth 0 mapping))
                (b (nth 1 mapping))
                (c (nth 2 mapping))
                (d (nth 3 mapping))
                (e (nth 4 mapping))
                (f (nth 5 mapping))
                (g (nth 6 mapping)))
                (print bin-num)
                (cond 
                        ((= bin-num (+ a b c e f g)) 0)
                        ((= bin-num (+ c f)) 1)
                        ((= bin-num (+ a c d e g)) 2)
                        ((= bin-num (+ a c d f g)) 3)
                        ((= bin-num (+ b c d f)) 4)
                        ((= bin-num (+ a b d f g)) 5)
                        ((= bin-num (+ a b d e f g)) 6)
                        ((= bin-num (+ a c f)) 7)
                        ((= bin-num (+ a b c d e f g)) 8)
                        ((= bin-num (+ a b c d f g)) 9)
                        ((= bin-num 0) 0)
                        (t "fail")
                )))

(defun process-line (operands results)
        (let* ( 
          (mapping (get-mappings (append operands results)))
          (result-nums (mapcar (lambda (num) (apply-mapping num mapping)) results)))
                (reduce (lambda (total x) (+ x (* 10 total))) result-nums)))

(defun problem2 (operands results)
        (apply #'+
                (mapcar #'process-line 
                        (mapcar #'wordlist-to-binlist operands) 
                        (mapcar #'wordlist-to-binlist results))))

(print (+ (count1478 (load-results))))
(print (problem2 (load-operands) (load-results)))
