(load "~/quicklisp/setup.lisp")
(require "split-sequence")
(require "alexandria")

(defun load-data() 
        (let* (
                (data (alexandria:read-file-into-string "input.txt"))
                (lines (split-sequence:split-sequence #\Newline data :remove-empty-subseqs t))
                (line (coerce (car lines) 'list)))
        (apply #'append (mapcar #'char-to-bits-p line))))

(defun char-to-bits-p (char)
    (cond 
        ((equal char #\0) (list 0 0 0 0))
        ((equal char #\1) (list 0 0 0 1))
        ((equal char #\2) (list 0 0 1 0))
        ((equal char #\3) (list 0 0 1 1))
        ((equal char #\4) (list 0 1 0 0))
        ((equal char #\5) (list 0 1 0 1))
        ((equal char #\6) (list 0 1 1 0))
        ((equal char #\7) (list 0 1 1 1))
        ((equal char #\8) (list 1 0 0 0))
        ((equal char #\9) (list 1 0 0 1))
        ((equal char #\A) (list 1 0 1 0))
        ((equal char #\B) (list 1 0 1 1))
        ((equal char #\C) (list 1 1 0 0))
        ((equal char #\D) (list 1 1 0 1))
        ((equal char #\E) (list 1 1 1 0))
        ((equal char #\F) (list 1 1 1 1))
))

; ---

(defun binlist-to-dec (binlist)
    (reduce
        (lambda (total x) (+ (* total 2) x))
        binlist))

(defun parse-literals-package-aux (binlist total)
    (let* ( (current-bits (subseq binlist 1 5))
            (current (binlist-to-dec current-bits))
            (rbinlist (subseq binlist 5))
            (new-total (+ (* total 16) current)))
        (if (= (nth 0 binlist) 1)
            (parse-literals-package-aux rbinlist new-total)
            (list new-total rbinlist))))

(defun parse-literals-package (binlist)
    (parse-literals-package-aux binlist 0))

(defun parse-len-subpackages (binlist)
    (let* ((p (parse-package binlist)))
        (if p
            (append (list (first p)) (parse-len-subpackages (second p)))
            nil)))

(defun parse-n-subpackages (binlist n)
    (if (= n 0)
        (list nil binlist)
        (let* ( (p (parse-package binlist))
                (pp (parse-n-subpackages (second p) (- n 1))))
            (list
                (append (list (first p)) (first pp))
                (second pp)))))

(defun parse-package (binlist)
    (if (< (length binlist) 11)
        nil
        (let* ( (version (binlist-to-dec (subseq binlist 0 3)))
                (typeid  (binlist-to-dec (subseq binlist 3 6)))
                (sub nil)
                (typ nil)
                (rbinlist nil))
            (if (= typeid 4)
                (let ((p (parse-literals-package (subseq binlist 6))))
                    (setq rbinlist (second p))
                    (setq typ 'literals)
                    (setq sub (first p)))
                (let*   ((I (nth 6 binlist))
                        (len-bits (if (= I 0) 15 11))
                        (len (binlist-to-dec (subseq binlist 7 (+ 7 len-bits))))
                        (rrbinlist (subseq binlist (+ 7 len-bits))))
                    (if (= I 0)
                        (let ((p (parse-len-subpackages (subseq rrbinlist 0 len))))
                            (setq rbinlist (subseq rrbinlist len))
                            (setq typ 'len-sub)
                            (setq sub p))
                        (let ((p (parse-n-subpackages rrbinlist len)))
                            (setq rbinlist (second p))
                            (setq typ 'n-sub)
                            (setq sub (first p))))))
            (list (list version typeid sub) rbinlist))))

(defparameter *packet* (first (parse-package (load-data))))

(defun sum-packet-version (packet)
    (+ 
        (first packet)
        (if (listp (third packet))
            (apply #'+
                (mapcar
                    #'sum-packet-version
                    (third packet)))
            0)))

(defun map-to-subs (func packet)
    (apply func (mapcar #'process-packet (third packet))))

(defun process-packet (packet)
    (let (  (typid (second packet))
            (content (third packet)))
        (cond 
            ((= typid 0) (map-to-subs #'+ packet))
            ((= typid 1) (map-to-subs #'* packet))
            ((= typid 2) (map-to-subs #'min packet))
            ((= typid 3) (map-to-subs #'max packet))
            ((= typid 4) content)
            ((= typid 5) (if (> (process-packet (first content)) (process-packet (second content))) 1 0))
            ((= typid 6) (if (< (process-packet (first content)) (process-packet (second content))) 1 0))
            ((= typid 7) (if (= (process-packet (first content)) (process-packet (second content))) 1 0)))))

(print *packet*)
(print (sum-packet-version *packet*))
(print (process-packet *packet*))

; A wrong: 22, 862 too low; 901 is solution
; B wrong: 8709715593 too low; 110434737925 correct
