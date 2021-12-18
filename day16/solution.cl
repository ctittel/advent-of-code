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

(defun get-version (binlist)
    (binlist-to-dec (subseq binlist 0 3)))

(defun type-id (binlist)
    (binlist-to-dec (subseq binlist 3 6)))

(defun parse-literals-package (binlist)
    (let* ((current (binlist-to-dec (subseq binlist 1 5))))
        (if (= (nth 0 binlist) 1)
            (+ (ash current 4) (parse-literals-package (subseq binlist 5)))
            (current))))

(defun parse-subpackages (binlist len)
    (append 
        (parse-package (subseq binlist 1 (+ 1 len)))
        (if (= 1 (nth 0 binlist))
            (parse-subpackages (subseq binlist (+ 1 len)) len)
            (list))))

(defun parse-n-subpackages (binlist len n)
    (append 
        (parse-package (subseq binlist 1 (+ 1 len)))
        (if (= 1 (nth 0 binlist))
            (parse-subpackages (subseq binlist (+ 1 len)) len)
            (list))))

(defun parse-package (binlist)
    (let* ( (version (binlist-to-dec (subseq binlist 0 3)))
            (typeid  (binlist-to-dec (subseq binlist 3 6))))
        (list version
            (if (= typeid 4)
                (parse-literal-package (subseq binlist 6))
                (let* ( (I (nth 6 binlist))
                        (len-bits    (if (= I 0) 15 11))
                        (len (binlist-to-dec (subseq binlist 7 (+ 7 len-bits))))
                        (rbinlist (subseq binlist (+ 7 len-bits))))
                    (if (= I 0)
                        (parse-subpackages (subseq rbinlist 0 len))
                        (parse-n-subpackages rbinlist 11 len)))))))

(print (load-data))
(parse-package (load-data))
