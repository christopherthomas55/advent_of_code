(ql:quickload :str)
(ql:quickload :alexandria)

(defmacro data () `(uiop:read-file-lines "10.txt"))

(defparameter *register* 1)
(defun add-register (val)
  (setf *register* (+ *register* val)))
(defparameter *output* (list))

;; Annoying cyle manipulation bs in middle
(defun main () (loop with cycle = 0 and val = NIL
      for op in (data)
      do (let ((split-words (str:words op)))
	   (progn
	     (print split-words)
	     (setf val NIL)
	     (setf cycle (+ 1 cycle))
	     ;; Some repeated code here
	     (if (zerop (mod (- cycle 20) 40))
		 (setf val (list cycle *register*))
		 NIL)
	     (if (not (equalp (first split-words) "noop"))
		 (progn (print *register*)
			(setf cycle (+ 1 cycle))
		        ;; ANd here
		        (if (zerop (mod (- cycle 20) 40))
			    (setf val (list cycle *register*))
			    NIL)
		        (add-register (parse-integer (second split-words))))
		 NIL)))
	     
      ;; Need branch here
      if (not (NULL val))
        ;; This won't work just placeholder. Probably need a temp variable at loop def
	collect val))

;; 1
(reduce #'+ (mapcar (lambda (x) (* (first x) (second x))) (main)))
     
;; 2
(defparameter *register* 1)
(defparameter *screen* (make-list 6 :initial-element (list)))
(defun draw-pixel (cycle)
  (let (
	  (rownum (floor cycle 40))
	  (val (if (<= (abs (- (mod cycle 40) *register*)) 1) "#" ".")))
  (progn (print (list cycle *register* val)) (setf (nth rownum *screen*) (append (nth rownum *screen*) (list val)))
  )))

;; Annoying cyle manipulation bs in middle
(defun main-2 () (loop with cycle = 0 and val = NIL
      for op in (data)
      for count from 0
      do (let ((split-words (str:words op)))
	   (progn
	     (setf val NIL)
	     (draw-pixel cycle)
	     (setf cycle (+ 1 cycle))
	     ;; Some repeated code here
	     (if (zerop (mod (- cycle 20) 40))
		 (setf val (list cycle *register*))
		 NIL)
	     (if (not (equalp (first split-words) "noop"))
		 (progn 
		        (draw-pixel cycle)
			(setf cycle (+ 1 cycle))
		        ;; ANd here
		        (if (zerop (mod (- cycle 20) 40))
			    (setf val (list cycle *register*))
			    NIL)
		        (add-register (parse-integer (second split-words))))
		 NIL)))
	     
      ;; Need branch here
      if (not (NULL val))
        ;; This won't work just placeholder. Probably need a temp variable at loop def
	collect val))

(defun combine-screen-row (x)
  (reduce (lambda (a b) (concatenate 'string a b)) x))

(progn (main-2) (print (mapcar #'combine-screen-row *screen*)))
     

		  

		  
