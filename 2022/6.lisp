(defmacro signal-data () `(coerce (uiop:read-file-string "6.txt") 'list))

;; n is number of elements to check
(defun unique (l n)
  (if (equalp n 0)
      T
    (if (member (car l) (cdr l))
      NIL
      (unique (append (cdr l) (list (car l))) (- n 1))
    )))

(defun first-different (previous-four inlist n)
  (if (unique previous-four n)
    n
    ;;else
    (first-different (nconc (cdr previous-four) (list (car inlist))) (cdr inlist) (+ n 1))))

(defun problem1 (data)
  (first-different (subseq data 0 4) (cddddr data) 4))

(defun problem2 (data)
  (first-different (subseq data 0 14) (nthcdr 14 data) 14))

(print (problem1 (signal-data)))
(print (problem2 (signal-data)))
