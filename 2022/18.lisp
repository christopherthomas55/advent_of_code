(ql:quickload :str)

;; An array really makes sense, but this is works and copied from last one
(defparameter *filled-spaces* (make-hash-table :test #'equalp))

(defun data-handler ()
  (mapcar (lambda (x) (mapcar #'parse-integer x))(mapcar (lambda (x) (str:split "," x)) (uiop:read-file-lines "18.txt"))))

(defun add-coords (x y)
  (list (+ (first x) (first y))
	(+ (second x) (second y))
	(+ (third x) (third y))))


(defun nearby-coords (x)
  (list (add-coords x '(1 0 0))
	(add-coords x '(-1 0 0))
	(add-coords x '(0 1 0))
	(add-coords x '(0 -1 0))
	(add-coords x '(0 0 1))
	(add-coords x '(0 0 -1))))

(defun store-coords (coords)
  (setf (gethash coords *filled-spaces*) T))


(defun exposed-sides (coords)
  (reduce #'+
	  (mapcar (lambda (x) (if (gethash x *filled-spaces*) 0 1))
		  (nearby-coords coords))))

(mapcar #'store-coords (data-handler))

;; 1
(print (reduce #'+ (mapcar #'exposed-sides (data-handler))))


;; 2 - Adding buffer of 1 for exterior search ease
(defparameter *minx* (- (reduce #'min (mapcar (lambda (x) (first x)) (data-handler))) 1))
(defparameter *maxx* (+ (reduce #'max (mapcar (lambda (x) (first x)) (data-handler))) 1))
(defparameter *miny* (- (reduce #'min (mapcar (lambda (x) (second x)) (data-handler))) 1))
(defparameter *maxy* (+ (reduce #'max (mapcar (lambda (x) (second x)) (data-handler))) 1))
(defparameter *minz* (- (reduce #'min (mapcar (lambda (x) (third x)) (data-handler))) 1))
(defparameter *maxz* (+ (reduce #'max (mapcar (lambda (x) (third x)) (data-handler))) 1))

(defun nearby-open (x)
  (remove-if (lambda (x) (gethash x *filled-spaces*)) (nearby-coords x)))
  

(defun inside-boundsp (coords)
  (and (<= *minx* (first coords)) (<= (first coords) *maxx*)
       (<= *miny* (second coords)) (<= (second coords) *maxy*)
       (<= *minz* (third coords)) (<= (third coords) *maxz*)))


(defparameter *exterior-spaces* (make-hash-table :test #'equalp))
(defun fill-exterior (stack-to-look)
  (let ((active (car stack-to-look)))
    (if (not active)
	NIL
	(if (and (not (gethash active *exterior-spaces*))
		 (inside-boundsp active))
	    (progn (setf (gethash active *exterior-spaces*) T)
		   (fill-exterior (append stack-to-look (nearby-open active))))
	    ;;else
	    (fill-exterior (cdr stack-to-look))))))

(fill-exterior (list (list *minx* *miny* *minz*)))

(defun exposed-exterior-sides (coords)
  (reduce #'+
	  (mapcar (lambda (x) (if (gethash x *exterior-spaces*)
				  1 0))
		  (nearby-coords coords))))


(print (reduce #'+ (mapcar #'exposed-exterior-sides (data-handler))))
