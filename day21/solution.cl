(load "~/quicklisp/setup.lisp")
(require "alexandria")

(defun roll-dice (dice)
    (apply #'+
        (mapcar
            (lambda (i) (+ (rem (+ dice i) 100) 1))
            (alexandria:iota 3))))

(defun play-until-victory (positions scores goal-score dice-rolls)
    (loop while (< (apply #'max scores) goal-score) do
        (let* ( (i (rem dice-rolls (length scores))) 
                (pos (nth i positions))
                (score (nth i scores))
                (rolled (roll-dice dice-rolls))
                (new-pos (rem (+ pos rolled) 10))
                (new-score (+ score new-pos 1)))
            (incf dice-rolls 3)
            (setf (nth i positions) new-pos)
            (setf (nth i scores) new-score)))
    (* (apply #'min scores) dice-rolls))

(defun problem1 ()
    (play-until-victory (list 1 7) (list 0 0) 1000 0))

(print (problem1))

;;; ---- Problem 2

(defun get-perms (s n)
    (if (= n 1)
        (mapcar #'list s)
        (apply #'append
            (loop for x in (get-perms s (- n 1)) collect
                (loop for ss in s collect
                    (append x (list ss)))))))

(defun get-perms2 (s n)
    (if (= n 1)
        s
        (apply #'append
            (loop for x in (get-perms2 s (- n 1)) collect
                (loop for ss in s collect
                    (+ x ss))))))

(defun get-wins-aux (scores positions cache)
    (apply #'mapcar #'+
        (apply #'append
            (loop for a in (get-perms2 (list 1 2 3) 3) collect
                (let* ( (new-posA (rem (+ (nth 0 positions) a) 10))
                        (new-scoresA (+ (nth 0 scores) new-posA 1)))
                    (if (>= new-scoresA 21) 
                        (list (list 1 0))
                        (loop for b in (get-perms2 (list 1 2 3) 3) collect
                            (let* ( (new-posB (rem (+ (nth 1 positions) b) 10))
                                    (new-scoresB (+ (nth 1 scores) new-posB 1)))
                                (if (>= new-scoresB 21) 
                                    (list 0 1)
                                    (get-wins (list new-scoresA new-scoresB) (list new-posA new-posB) cache))))))))))

(defun get-wins (scores positions cache)
    (let (  (key (list scores positions)))
        (if (null (gethash key cache))
            (setf (gethash key cache) (get-wins-aux scores positions cache))
            nil)
        (gethash key cache)))

(defun problem2 ()
    (apply #'max
        (print (get-wins (list 0 0) (list 1 7) (make-hash-table :test 'equal)))))

(print (problem2))

; inputs:
; Player 1 starting position: 2
; Player 2 starting position: 8
