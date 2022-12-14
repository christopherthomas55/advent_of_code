(ql:quickload :str)

;; Data confirmed working on test input
(defun data-handler ()
  (defun remove-> (list) (remove "->" list :test #'string=))
  (defun to-coords (list) (mapcar (lambda (x) (mapcar #'parse-integer (str:words x)))
			     (mapcar (lambda (x) (substitute #\SPACE #\, x)) list)))
  (mapcar #'to-coords (mapcar #'remove-> (mapcar #'str:words (uiop:read-file-lines "14.txt")))))

;; To normalize input
(defun int-min (a b) (if (< a b) a b))
(defun int-max (a b) (if (> a b) a b))
(defparameter *smallestx* (reduce #'int-min (loop for el in (data-handler)
      with sublist = (list)
      do (setf sublist (append sublist (mapcar #'first el)))
      finally (return sublist))))

(defparameter *largestx* (reduce #'int-max (loop for el in (data-handler)
      with sublist = (list)
      do (setf sublist (append sublist (mapcar #'first el)))
      finally (return sublist))))

(defparameter *sandstart* (list (- 500 *smallestx*) 0))
(defparameter *largesty* (reduce #'int-max (loop for el in (data-handler)
      with sublist = (list)
      do (setf sublist (append sublist (mapcar #'second el)))
      finally (return sublist))))
(defparameter *xsize* (+ 1 (- *largestx* *smallestx*)))
(defparameter *ysize* (+ 1 *largesty*))

;; idx direction |->
;;               v
(defparameter *filled* (loop for j below *ysize* collect (loop for i below *xsize* collect 0)))

(defun between (a b out)
  (cond ((not (equalp (first a) (first b)))
	 (let ((add-this (/ (- (first b) (first a)) (abs (- (first b) (first a))))))
	   (between (list (+ add-this (first a)) (second a)) b (append out (list a)))))
	((not (equalp (second a) (second b)))
	 (let ((add-this (/ (- (second b) (second a)) (abs (- (second b) (second a))))))
	   (between (list (first a) (+ add-this (second a))) b (append out (list a)))))
	(T (append out (list b)))))

(defun fill-from-el (list)
  (if (not (cadr list))
      NIL
      (let ((first (car list)) (second (cadr list)))
	(progn (loop for x in (between first second NIL)
		     do (setf (nth (- (first x) *smallestx*) (nth (second x) *filled*)) 1))
	       (fill-from-el (cdr list))))))

(mapcar #'fill-from-el (data-handler))

(defun add-coords (a b) (list (+ (first a) (first b)) (+ (second a) (second b))))
(defun at-location (location) (nth (first location) (nth (second location) *filled*)))
(defun set-location (location val) (setf (nth (first location) (nth (second location) *filled*)) val))
(defun sand-drops (location rested)
  ;; 5 cases. Off map. Bottom open. Left open. Right open. None open
  (cond
    ;; Off map here
    ((not (at-location location)) rested)

    ;; Bottom open
    ((zerop (at-location (add-coords location '(0 1))))
	   (sand-drops (add-coords location '(0 1)) rested))

    ;; Left open
    ((zerop (at-location (add-coords location '(-1 1))))
	   (sand-drops (add-coords location '(-1 1)) rested))

    ;;Right
    ((zerop (at-location (add-coords location '(1 1))))
	   (sand-drops (add-coords location '(1 1)) rested))

    (T (progn (set-location location 8) (sand-drops *sandstart* (+ rested 1))))))
      
;; Part 1
;; So uh, this errors out and says the value. But it's right lmao
;;(sand-drops *sandstart* 0)

(loop for x in *filled* do (print x))

(defparameter *largesty* (+ 2 (reduce #'int-max (loop for el in (data-handler)
      with sublist = (list)
      do (setf sublist (append sublist (mapcar #'second el)))
      finally (return sublist)))))

;; Redoing some params here
(defparameter *smallestx* (- 500 *largesty*)) 
(defparameter *largestx*  (+ 500 *largesty*)) 

(defparameter *sandstart* (list (- 500 *smallestx*) 0))
(defparameter *xsize* (+ 1 (- *largestx* *smallestx*)))
(defparameter *ysize* (+ 1 *largesty*))

;; idx direction |->
;;               v
(defparameter *filled* (loop for j below *ysize* collect (loop for i below *xsize* collect 0)))

(defun sand-drops (location rested)
  ;; 5 cases. Overlap. Bottom open. Left open. Right open. None open
  (cond
    ;; Nowhere to go
    ((equalp 8 (at-location location)) rested)

    ;; Bottom open
    ((zerop (at-location (add-coords location '(0 1))))
	   (sand-drops (add-coords location '(0 1)) rested))

    ;; Left open
    ((zerop (at-location (add-coords location '(-1 1))))
	   (sand-drops (add-coords location '(-1 1)) rested))

    ;;Right
    ((zerop (at-location (add-coords location '(1 1))))
	   (sand-drops (add-coords location '(1 1)) rested))

    (T (progn (set-location location 8) (sand-drops *sandstart* (+ rested 1))))))
      


(mapcar #'fill-from-el (append (data-handler) (list (list (list *smallestx* *largesty*) (list *largestx* *largesty*)))))

;; This one doesn't error out
(print (sand-drops *sandstart* 0))
