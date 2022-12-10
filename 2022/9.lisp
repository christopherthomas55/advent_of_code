(ql:quickload :str)
(ql:quickload :alexandria)

(defmacro move-data () `(uiop:read-file-lines "9.txt"))
(defparameter *rope* (list (list 0 0) (list 0 0)))
(defparameter *visited* (list (list 0 0)))

(defun mv-cmd (word-list)
  (let ((cmd (first word-list)) (val (parse-integer (second word-list))))
    (if (equalp cmd "U")
    (make-list val :initial-element (list 0 1))
    (if (equalp cmd "D")
    (make-list val :initial-element (list 0 -1))
    (if (equalp cmd "R")
    (make-list val :initial-element (list 1 0))
    (if (equalp cmd "L")
    (make-list val :initial-element (list -1 0))
    NIL))))))

(defun flatten-once (list)
  (loop for el in list
	nconc el))
     
(defun is-close (coordsa coordsb)
  (and (<= (abs (- (first coordsa) (first coordsb))) 1)
       (<= (abs (- (second coordsa) (second coordsb))) 1)))


(defun add-coords (a b)
  (list (+ (first a) (first b)) (+ (second a) (second b))))


(defun propagate-mv (i infront-was-here infront-now-here)
  (if (equalp i 0)
    (progn (setf (first *rope*) infront-now-here) (propagate-mv 1 infront-was-here infront-now-here))
  (if (equalp i (length *rope*))
      (setf *visited* (append  *visited* (copy-list (last *rope*))))
  (let ((close (is-close (nth i *rope*) infront-now-here)) (old-spot (nth i *rope*)))
    (if (not close)
      (progn (setf (nth i *rope*) infront-was-here) (propagate-mv (+ 1 i) old-spot infront-was-here))
     NIL)))))
		      
(defun do-mv (cmd-list)
  (if (not cmd-list)
    NIL
    (progn (propagate-mv 0 (first *rope*) (add-coords (first *rope*) (car cmd-list)))
	   (do-mv (cdr cmd-list)))))

(do-mv (flatten-once (mapcar #'mv-cmd (mapcar #'str:words (move-data)))))

;; problem 1
(print (length (remove-duplicates *visited* :test #'equalp)))

;; problem 2. Oof I misunderstood the intricacies of rope physics
(defparameter *rope* (make-list 10 :initial-element (list 0 0)))
(defparameter *visited* (list (list 0 0)))

(defun is-diag (coordsa coordsb)
  (or (and (> (abs (- (first coordsa) (first coordsb))) 1)
	   (>= (abs (- (second coordsa) (second coordsb))) 1))
       (and (>= (abs (- (first coordsa) (first coordsb))) 1)
	   (> (abs (- (second coordsa) (second coordsb))) 1))))

(defun diff (coordsa coordsb)
  (list (- (first coordsa) (first coordsb)) (- (second coordsa) (second coordsb))))

(defun dir-large (coords) (and (equalp 1 (abs (first coords))) (equalp 1 (abs (second coords)))))

(defun one-in-dir (coords) (if (/= (first coords) 0)
  (list (/ (abs (first coords)) (first coords)) 0)
  (list 0 (/ (abs (second coords)) (second coords)))
))

;; This was a pain jesus. My diag function was wrong so I destroyed this structure to get it
(defun propagate-mv-2 (i infront-was-here infront-now-here direction)
  (if (equalp i 0)
    (progn (setf (first *rope*) infront-now-here) (propagate-mv-2 1 infront-was-here infront-now-here direction))
  (if (equalp i (length *rope*))
      (setf *visited* (append  *visited* (copy-list (last *rope*))))
  (let ((close (is-close (nth i *rope*) infront-now-here)) (old-spot (nth i *rope*)))
    (let ((diag (is-diag old-spot infront-now-here)))
    (if (not close)
     (if (not diag) 
      (let ((new-spot (add-coords old-spot (one-in-dir (diff infront-now-here old-spot)))))
	(progn (setf (nth i *rope*) new-spot) (propagate-mv-2 (+ 1 i) old-spot new-spot (diff new-spot old-spot))))
      (if (dir-large direction)
        (let ((new-spot (add-coords old-spot direction)))
	(progn (setf (nth i *rope*) new-spot) (propagate-mv-2 (+ 1 i) old-spot new-spot (diff new-spot old-spot))))
      ;;else
      (let ((new-spot infront-was-here))
      (progn (setf (nth i *rope*) new-spot) (propagate-mv-2 (+ 1 i) old-spot new-spot (diff new-spot old-spot))))))))))))

		      
(defun do-mv-2 (cmd-list)
  (if (not cmd-list)
    NIL
    (progn (propagate-mv-2 0 (first *rope*) (add-coords (first *rope*) (car cmd-list)) (car cmd-list))
	   (do-mv-2 (cdr cmd-list)))))

(do-mv-2 (flatten-once (mapcar #'mv-cmd (mapcar #'str:words (move-data)))))


(print (length (remove-duplicates *visited* :test #'equalp)))
