(ql:quickload :str)

(defmacro data () `(uiop:read-file-lines "4.txt"))


(defun separate-str (instr sep)
  (loop with substring = "" for el across instr
    if (equalp el sep)
      nconc (list substring) into res and do (setf substring "")
    else
      do (setf substring (str:concat substring (string el)))
    finally
      (progn (nconc res (list substring)) (return res))))

;; Thought about writing a macro for this
(defun dash-sep-wrapper (str)
  (separate-str str #\-))

(defun comma-sep-wrapper (str)
  (separate-str str #\,))

(defun sublist-dash-sep-wrapper (strlist)
  (mapcar #'dash-sep-wrapper strlist))

(defun str-conversion-wrapper (strlist)
  (mapcar (lambda (sublist) (mapcar #'parse-integer sublist)) strlist))

(defun logic-handler (two_list)
  (let (
    (a (car two_list))
    (b (cadr two_list))
   )
    ;; Ugly way of representing two possibilities
    (if (or (and (<= (car a) (car b)) (>= (cadr a) (cadr b)))
	    (and (<= (car b) (car a)) (>= (cadr b) (cadr a))))
      1 0)))
   

;; Problem 1
(reduce #'+
  (mapcar #'logic-handler
    (mapcar #'str-conversion-wrapper
      (mapcar #'sublist-dash-sep-wrapper
        (mapcar #'comma-sep-wrapper (data))))))


(defun logic-handler-2 (two_list)
  (let (
    (a (car two_list))
    (b (cadr two_list))
   )
    ;; Ugly way of representing two possibilities
    (if (or (and (<= (car a) (car b)) (>= (cadr a) (car b)))
	    (and (<= (car b) (car a)) (>= (cadr b) (car a))))
      1 0)))


;; Problem 2
(reduce #'+
  (mapcar #'logic-handler-2
    (mapcar #'str-conversion-wrapper
      (mapcar #'sublist-dash-sep-wrapper
        (mapcar #'comma-sep-wrapper (data))))))
