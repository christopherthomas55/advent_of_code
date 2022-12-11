;; Ah fuck this parsing is too much time. I am gonna write the logic in manually
;; 1.Starting items, 2.Operation, 3.Test, 4.True condition, 5.False condition
;; Think will need a small macro on operation
;; Can write parser later if need be
;; Annoying but to count and make mapping easier I'm putting monkey num at end to transfer it
(defparameter *data* '(
  ;;0
  ((59 74 65 86) (lambda (old) (* old 19)) 7 6 2 0)
  ;;1
  ((62 84 72 91 68 78 51) (lambda (old) (+ old 1)) 2 2 0 1)
  ;;2
  ((78 84 96) (lambda (old) (+ old 8)) 19 6 5 2)
  ;;3
  ((97 86) (lambda (old) (* old old)) 3 1 0 3)
  ;;4
  ((50) (lambda (old) (+ old 6)) 13 3 1 4)
  ;;5
  ((73 65 69 65 51) (lambda (old) (* old 17)) 11 4 7 5)
  ;;6
  ((69 82 97 93 82 84 58 63) (lambda (old) (+ old 5)) 5 5 7 6)
  ;;7
  ((81 78 82 76 79 80) (lambda (old) (+ old 3)) 17 3 4 7)
  )
)

(defparameter *test* '(
  ;;0
  ((79 98) (lambda (old) (* old 19)) 23 2 3 0)
  ;;1
  ((54 65 75 74) (lambda (old) (+ old 6)) 19 2 0 1)
  ;;2
  ((79 60 97) (lambda (old) (* old old)) 13 1 3 2)
  ;;3
  ((74) (lambda (old) (+ old 3)) 17 0 1 3)
  )
)

(defparameter *test-inspection-count* (make-list 4 :initial-element 0))
(defparameter *data-inspection-count* (make-list 8 :initial-element 0))

(defun move-spot-val-to-monkey-j (val j)
  ;; Costly to traverse end and add to it, but fine at this scale. So will just do heavy handed append
  (setf (first (nth j *data*)) (append (first (nth j *data*)) (list val))))

;; We are lambda-ing by old to match with data
;;(defun calc-macro (val stmt)
;;  ((lambda (old) (eval stmt)) val))
;;
;;(calc-macro 1 (* old 5))
;;
;;(defmacro divby-macro (val stmt)
;;  `(zerop (mod ,val ',stmt)))
;;
;;(macroexpand '(divby-macro x (divby 19)))
;; (zerop (mod val divnum)

;; Want calc macro to run on each
;;(defun macro-test (monkey-data)
;;  (calc-macro 10 (second monkey-data)))
;;(mapcar #'macro-test *test*)

(defun divby (val check)
  (zerop (mod val check)))

;; Make sure to use pointers here! F. Edit - we didn't need to use pointers lol
(defun round-apply (monkey-input)
  ;; Here we apply the eval-d calc function and divide by 3 all in the same step since it happens no matter what. Note: floor is "traditional" int division in other languages
  (let ((applied_transforms
	  (mapcar (lambda (x) (floor (funcall (eval (second monkey-input)) x) 3)) (first monkey-input))))
    (loop for el in applied_transforms
          for y from 1
	  ;; COunt inspection, then move around
	  do (setf (nth (sixth monkey-input) *data-inspection-count*)
		   (+ 1 (nth (sixth monkey-input) *data-inspection-count*)))
	     (if (divby el (third monkey-input))
	       (move-spot-val-to-monkey-j el (fourth monkey-input))
	       ;;else
	       (move-spot-val-to-monkey-j el (fifth monkey-input))))))


;;works
(defun mult-max-2(maxa maxb remaining_list)
  (if (not remaining_list)
      (* maxa maxb)
      (let ((x (sort (list maxa maxb (car remaining_list)) #'>)))
      (mult-max-2 (first x) (second x) (cdr remaining_list)))))


;; 1 - gotta comment out cause I did the hackiest shit of all time - breaking to escape loop
(loop for counter from 1
  do (loop for el in *test* ;;*data*
           for which_monkey from 0
             ;; Make sure to set monkeys items to 0 after processing!
	     do (round-apply el) (setf (first (nth which_monkey *test*)) (list)) (print *test*))
     (if (>= counter 20)
	 (progn (print *test-inspection-count*)
		(print (mult-max-2 0 0 *test-inspection-count*))
		(error "DONE")) ;;test or real
      NIL))

(defparameter *mod-value*
  (reduce #'* (mapcar (lambda (x) (third x)) *data*)))
     
;; Only diff so far is changing floor to mod *mod-value*
(defun round-apply-2 (monkey-input)
  (let ((applied_transforms
	  (mapcar (lambda (x) (mod (funcall (eval (second monkey-input)) x) *mod-value*)) (first monkey-input))))
    (loop for el in applied_transforms
          for y from 1
	  ;; COunt inspection, then move around
	  do (setf (nth (sixth monkey-input) *data-inspection-count*)
		   (+ 1 (nth (sixth monkey-input) *data-inspection-count*)))
	     (if (divby el (third monkey-input))
	       (move-spot-val-to-monkey-j el (fourth monkey-input))
	       ;;else
	       (move-spot-val-to-monkey-j el (fifth monkey-input))))))

;; 2
(loop for counter from 1
  do (loop for el in *data* ;;*data*
           for which_monkey from 0
             ;; Make sure to set monkeys items to 0 after processing!
	     do (round-apply-2 el) (setf (first (nth which_monkey *data*)) (list)))
     (if (equalp counter 1) (progn (print counter) (print *data-inspection-count*)) NIL)
     (if (zerop (mod counter 1000)) (progn (print counter) (print *data-inspection-count*)) NIL)
     (if (>= counter 10000)
	 (progn (print *data-inspection-count*)
		(print (mult-max-2 0 0 *data-inspection-count*))
		(error "DONE")) ;;test or real
      NIL)))

    
 
