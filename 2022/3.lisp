(defmacro data () `(uiop:read-file-lines "3.txt"))

;;a-z = 97 - 122
;;A-Z = 65 -  90
(defun char-val (char)
  (let((val (char-code char)))
    (if (< val 91)
      (- val 38)
     ;;else
      (- val 96))))

(defun split-str (str)
  (list
   (coerce (subseq str 0 (/ (length str) 2)) 'list)
   (coerce (subseq str (/ (length str) 2)) 'list)))

;; Nice built in intersection returns list of common
(defun intersection-wrapper (element)
  (car (intersection (car element) (cadr element))))

;; Problem 1
(print
  (reduce #'+
    (mapcar #'char-val
      (mapcar #'intersection-wrapper
	(mapcar #'split-str (data))))))


;; Problem 2

;; Lazy way of extending intersection to 3 elements. Many more elegant ways to do this
(defun intersection-wrapper-2 (element)
  (car (intersection
	(caddr element)
	(intersection (car element) (cadr element)))))

(defun group-data (input-lines)
  (loop with sublist for el in input-lines
    for n from 1
    do (push (coerce el 'list) sublist)
    if (equalp (mod n 3) 0)
      collect sublist and do (setf n 0) (setf sublist nil)
  ))

(print
  (reduce #'+
    (mapcar #'char-val
      (mapcar #'intersection-wrapper-2
	(group-data (data))))))
