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
            (print (list "i" i "n" dice-rolls "poss" positions "scores" scores "roll" rolled "npos" new-pos "nscore" new-score))
            (setf (nth i positions) new-pos)
            (setf (nth i scores) new-score)))
    (print (list dice-rolls positions scores))
    (* (apply #'min scores) dice-rolls))

(defun problem1 ()
    (play-until-victory (list 1 7) (list 0 0) 1000 0))

(print (problem1))
