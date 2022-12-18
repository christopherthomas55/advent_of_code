(defparameter *actions*
  (remove #\Newline
	  (coerce (uiop:read-file-string "17.txt") 'list)
	  :test 'equalp))
;; Make circular list
(setf (cdr (last *actions*)) *actions*)

;; SO we can actually pring circular lists
(setf *print-circle* t)

(defparameter *minx* 0)
(defparameter *maxx* 6)
;; Defines offsets froms start. In order of falling
;; Circular list too, how cool
(defparameter *rocks* '#1=(
			((0 0) (1 0) (2 0) (3 0))
			((1 0) (0 1) (1 1) (2 1) (1 2))
			((0 0) (1 0) (2 0) (2 1) (2 2))
			((0 0) (0 1) (0 2) (0 3))
			((0 0) (0 1) (1 0) (1 1)) . #1#)
	                )


;; Storing all spaces as a hash table. Even has custom hash fgunction
(defparameter *filled-spaces* (make-hash-table :test #'equalp))

;; Track max heights to be slightly more efficient
(defparameter *max-heights* '(0 0 0 0 0 0 0))

(defun reinit ()
  (defparameter *max-heights* '(0 0 0 0 0 0 0))
  (defparameter *filled-spaces* (make-hash-table :test #'equalp)))

(reinit)

(defun new-rock (rock-base)
  (mapcar (lambda (x) (list (+ 2 (first x)) (+ 4 (reduce #'max *max-heights*) (second x)))) rock-base))
	      
(defun down-move (rock)
  (loop for coord in rock
	if (or
	    (equalp 1 (- (second coord) (nth (first coord) *max-heights*)))
	    (gethash coord *filled-spaces*))
            do (return NIL)
        finally
	  (return (mapcar (lambda (x) (list (first x) (- (second x) 1))) rock))))
       
(defun rock-now-max (rock)
  (if (not rock) NIL
    (let ((x (first (car rock))) (y (second (car rock))))
      (progn (if (> y (nth x *max-heights*)) (setf (nth x *max-heights*) y) NIL)
	(rock-now-max (cdr rock))))))
	
      
(defun stop-rock (rock)
  (progn (mapcar (lambda (x) (setf (gethash x *filled-spaces*) T)) rock)
	 (rock-now-max rock)))
    

(defun add-coords (x y) (list (+ (first x) (first y)) (+ (second x) (second y))))

(defun hits-wall (coords) (if (or (< (first coords) *minx*) (> (first coords) *maxx*)) T NIL))

(defun do-op (op rock)
  (let ((xchange (if (equalp op #\<) '(-1 0) '(1 0))))
    (loop for el in rock 
       if (hits-wall (add-coords el xchange)) 
	 do (return rock) 
       if (gethash (add-coords el xchange) *filled-spaces*)

	 do (return rock) 
       finally
	 (return (mapcar (lambda (x) (add-coords x xchange)) rock)))))
	  

;; Apply move, then fall
;; Returns the cdr of where we're at for operation
(defun apply-operations (operation rock count)
  (let ((applied-rock (do-op (car operation) rock)))
    (let ((down-one (down-move applied-rock)))

      ;; 365
      ;; if applied op is same and no down one, we can't do operatioh and don't cdr
      (if (not down-one) (progn (stop-rock applied-rock) (cdr operation))
      (apply-operations (cdr operation) down-one (+ count 1))))))

(defun do-rock (rock-car operation-car n numtimes)
  (if (>= n numtimes) NIL 
      (let ((instantiated-rock (new-rock (car rock-car))))
	(let ((operation (apply-operations operation-car instantiated-rock 0)))
	  (do-rock (cdr rock-car) operation (+ n 1) numtimes)))))

;; 1
(progn (do-rock *rocks* *actions* 0 2022)
	(print *max-heights*)
	(print (reduce #'max *max-heights*)))


(defun all-equalp (l)
  (if (not (cadr l))
      T 
      (if (equalp (car l) (cadr l))
	  (all-equalp (cdr l))
	  NIL)))

;; Need to reload cause cicular list
(defparameter *op-count*
  (length (remove #\Newline
	  (coerce (uiop:read-file-string "17.txt") 'list)
	  :test 'equalp)))



(defparameter *flats* (make-hash-table :test #'equalp))
(setf (gethash '(0 0) *filled-spaces*) 0)

(defparameter *target* 1000000000000)

	
(defun last-calc (height prev-height rock-i op-i)
  (let ((cycle-len (- height prev-height)))
    (let ((calcs-left  (mod (- *target* height) cycle-len))
	  (height-at-last (+  (* (floor (- *target* height) cycle-len)
			          cycle-len 
				  )
			      prev-height)))
      (progn (reinit)
	     (do-rock (nthcdr rock-i *rocks*) (nthcdr op-i *actions*) 0 calcs-left)
	     (error (+ (reduce #'max *max-heights*) height-at-last))))))


(defun handle-flats (rock-i op-i height)
  (let ((seen? (gethash (list rock-i op-i) *flats*)))
    (if seen?
	(last-calc height seen? rock-i op-i)
	(setf (gethash (list rock-i op-i)  *flats*) height))))


(defun do-rock-2 (rock-car operation-car n)
  (progn
    (if (all-equalp *max-heights*)
	(progn (handle-flats (mod n 5) (mod n *op-count*) (reduce #'max *max-heights*)))
	NIL)
    (if (zerop (mod n 1000000)) (print  n) NIL)
    (let ((instantiated-rock (new-rock (car rock-car))))
       (let ((operation (apply-operations operation-car instantiated-rock 0)))
	  (do-rock-2 (cdr rock-car) operation (+ n 1))))))

(progn (do-rock-2 *rocks* *actions* 0))
