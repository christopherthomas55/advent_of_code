(defparameter *input* (mapcar (lambda (x) (* x 811589153)) (mapcar #'parse-integer (uiop:read-file-lines "20.txt"))))

(defparameter *length* (length *input*))
(defparameter *active* (loop for n below *length* collect n))


;; Need this since numbers not uniqeu
(defparameter *hashthing* (make-hash-table :test #'equalp))

(defparameter *number-hash* (loop for n below *length*
				  do (setf (gethash n *hashthing*) (nth n *input*))))

(loop for j below 10 do (print "---") (print (mapcar (lambda (x) (gethash x *hashthing*)) *active*))
 (loop for i below (length *input*)
  do ;;(print "---") (print (mapcar (lambda (x) (gethash x *hashthing*)) *active*)) (print (gethash i *hashthing*))
  (let ((num (gethash i *hashthing*)))
  (let ((current-loc (position i *active*)))
  (let ((target-loc-temp (+ num current-loc)))
  ;; Annoying off by one crap
  (let ((target-loc
	  (if (<= target-loc-temp 0)
	      (mod (+ target-loc-temp (floor (- target-loc-temp 1) *length*)) *length*)
	      (if (>= target-loc-temp *length*)
	        (mod (+ target-loc-temp (floor (+ target-loc-temp 1) *length*)) *length*)
		(mod target-loc-temp *length*)))))
  (let ((target-loc-num (nth target-loc *active*)))
    ;; First add, then remove extra number from where
      (progn 
      (if (<= current-loc target-loc)
          (let ((added-inplace
	      (append (subseq *active* 0 (+ 1 target-loc)) (cons i (nthcdr (+ 1 target-loc) *active*)))))
	      (setf *active* (append (subseq added-inplace 0 current-loc)
				  (nthcdr (+ 1 current-loc) added-inplace))))
        (let ((added-inplace
	      (append (subseq *active* 0 target-loc) (cons i (nthcdr target-loc *active*)))))
	      (setf *active* (append (subseq added-inplace 0 (+ 1 current-loc))
				     (nthcdr (+ 2 current-loc) added-inplace)))))
      ;;(print (mapcar (lambda (x) (gethash x *hashthing*)) *active*))
      ))))))))



(defparameter zero-i (loop for x below *length* if (zerop (gethash x *hashthing*)) do (return x)))

(let ((pos-0 (position zero-i *active*)))
  (print (list
	  (gethash (nth (mod (+ pos-0 1000) *length*) *active*) *hashthing*)
	  (gethash (nth (mod (+ pos-0 2000) *length*) *active*) *hashthing*)
	  (gethash (nth (mod (+ pos-0 3000) *length*) *active*) *hashthing*))))

(let ((pos-0 (position zero-i *active*)))
  (print (+
	  (gethash (nth (mod (+ pos-0 1000) *length*) *active*) *hashthing*)
	  (gethash (nth (mod (+ pos-0 2000) *length*) *active*) *hashthing*)
	  (gethash (nth (mod (+ pos-0 3000) *length*) *active*) *hashthing*))))


;; Only works for 1 ugh
