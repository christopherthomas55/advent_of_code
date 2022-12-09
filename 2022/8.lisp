(defmacro tree-data () `(uiop:read-file-lines "8.txt"))
(defun coerce-str-intlist (instr) (mapcar #'digit-char-p (coerce instr 'list)))
(defun tree-listified ()
  (mapcar #'coerce-str-intlist (tree-data)))

(print (tree-listified))
(defparameter *trees* (tree-listified))
(defparameter *ylen* (length *trees*))
(defparameter *xlen* (length (first *trees*)))
(defun intmax (a b)(if (> a b) a b))

(defun larger-than-max (val list) (> val (reduce #'intmax list)))
(defun is-visible (x_i y_i)
  (if (or (equalp x_i 0) (equalp x_i (- *xlen* 1)) (equalp y_i 0) (equalp y_i (- *ylen* 1)))
    T
    (let ((val (nth x_i (nth y_i *trees*))))
          ;; all to right
      (or (larger-than-max val (nthcdr (+ x_i 1) (nth y_i *trees*)))
          ;; all to left
	  (larger-than-max val (subseq (nth y_i *trees*) 0 x_i))
	  ;; above
	  (larger-than-max val (subseq (mapcar (lambda (x) (nth x_i x)) *trees*) 0 y_i))
	  ;; below
          (larger-than-max val (nthcdr (+ y_i 1) (mapcar (lambda (x) (nth x_i x)) *trees*)))
))))

;; Hacky way to loop through positions
(defun main ()
  (loop for y in *trees*
        for y_i from 0
        collect (loop for x in (nth y_i *trees*)
	      for x_i from 0
	      if (is-visible x_i y_i)
	        collect (list x_i y_i))))

;; Answer 1
(print (reduce #'+ (mapcar #'length (main))))


(defun first-larger (val n list)
  (if (not list)
    n
    (if (>= (car list) val)
      (+ n 1)
      (first-larger val (+ n 1) (cdr list)))))
	
(defun scenic-score (x_i y_i)
  (if (or (equalp x_i 0) (equalp x_i (- *xlen* 1)) (equalp y_i 0) (equalp y_i (- *ylen* 1)))
    0
    (let ((val (nth x_i (nth y_i *trees*))))
          ;; all to right
      (* (first-larger val 0 (nthcdr (+ x_i 1) (nth y_i *trees*)))
          ;; all to left
	  (first-larger val 0 (reverse (subseq (nth y_i *trees*) 0 x_i)))
	  ;; above
	  (first-larger val 0 (reverse (subseq (mapcar (lambda (x) (nth x_i x)) *trees*) 0 y_i)))
	  ;; below
          (first-larger val 0 (nthcdr (+ y_i 1) (mapcar (lambda (x) (nth x_i x)) *trees*)))))))

(defun main-2 ()
  (loop for y in *trees*
        for y_i from 0
        collect (loop for x in (nth y_i *trees*)
	      for x_i from 0
	        collect (scenic-score x_i y_i))))

;; Answer 2
(print (reduce #'intmax (mapcar (lambda (x) (reduce #'intmax x)) (main-2))))
