;; Cheating and not parsing puzzle input
;; Location the closest beacon
;; This seems to be building towards counting all spots
(defparameter *test-data*
  `(((2 18) (-2 15))
    ((9 16) (10 16))
    ((13 2) (15 3))
    ((12 14) (10 16))
    ((10 20) (10 16))
    ((14 17) (10 16))
    ((8 7) (2 10))
    ((2 0) (2 10))
    ((0 11) (2 10))
    ((20 14) (25 17))
    ((17 20) (21 22))
    ((16 7) (15 3))
    ((14 3) (15 3))
    ((20 1) (15 3)))
 )

(defparameter *in-data*
  '(((3729579 1453415) (4078883 2522671))
    ((3662668 2749205) (4078883 2522671))
    ((257356 175834) (1207332 429175))
    ((2502777 3970934) (3102959 3443573))
    ((24076 2510696) (274522 2000000))
    ((3163363 3448163) (3102959 3443573))
    ((1011369 447686) (1207332 429175))
    ((3954188 3117617) (4078883 2522671))
    ((3480746 3150039) (3301559 3383795))
    ((2999116 3137910) (3102959 3443573))
    ((3546198 462510) (3283798 -405749))
    ((650838 1255586) (274522 2000000))
    ((3231242 3342921) (3301559 3383795))
    ((1337998 31701) (1207332 429175))
    ((1184009 3259703) (2677313 2951659))
    ((212559 1737114) (274522 2000000))
    ((161020 2251470) (274522 2000000))
    ((3744187 3722432) (3301559 3383795))
    ((2318112 2254019) (2677313 2951659))
    ((2554810 56579) (3283798 -405749))
    ((1240184 897870) (1207332 429175))
    ((2971747 2662873) (2677313 2951659))
    ((3213584 3463821) (3102959 3443573))
    ((37652 3969055) (-615866 3091738))
    ((1804153 1170987) (1207332 429175)))
 )
 

(defparameter *data* *in-data*)

(defun distance (a b)
  (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))

(defparameter *sensors* (mapcar (lambda (x) (list (first x) (distance (first x) (second x)))) *data*))

(defparameter *beacons* (remove-duplicates (mapcar (lambda (x) (second x)) *data*) :test #'equalp))
*beacons*

(defun interval-overlap? (a b)
  (if (or
        (and (<= (first a) (first b)) (>= (second a) (first b)))
        (and (<= (first b) (first a)) (>= (second b) (first a))))
      T
      NIL))

(defun combine-interval (a b)
  (list (min (first a) (first b)) (max (second a) (second b))))

;; Recursive check if testi matches anywhere in il list
(defun overlapping-where (il testi n)
  (cond ((not il) NIL)
	((interval-overlap? (first il) testi) n)
	(T (overlapping-where (cdr il) testi (+ n 1)))))

;; Copied from stackoverflow. I'm tired of doing basic shit sometimes. I wish this was common libr
;;https://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))
 
;; Note - not sorted. Naive recursive way to reduce intervals
;; If match on any in "new_intervals" we combine and start with that as first
;; At end should be no overlapping intervals	  
;; Overly complicated

(defun reduce-intervals (intervals new_intervals)
  (progn ;;(print intervals) (print new_intervals)
  (if (not intervals) new_intervals
  (if (not new_intervals) (reduce-intervals (cdr intervals) (list (car intervals)))
  (let (( overlap-i (overlapping-where new_intervals (car intervals) 0)))
    (cond
	;; If overlap. Very inefficient to find overlap 3 times
	(overlap-i 
	 (reduce-intervals (append (cdr intervals) (remove-nth overlap-i new_intervals))
			   (list (combine-interval (nth overlap-i new_intervals) (car intervals)))))
	;; No overlap at all
	(T (reduce-intervals (cdr intervals) (append new_intervals (list (car intervals)))))))))))

(defun intervals-in-row-n (n sensors intervals)
   ;; No sensors left means done
  (if (not sensors) intervals
  (let ((sensor (car sensors))
	(row-dist (abs (- n (cadaar sensors))))
	(free-moves (abs (- (cadar sensors) (abs (- n (cadaar sensors)))))))
    (cond 
	  ;; If sensor farther from row than beacon, skip
          ((> row-dist (second sensor)) (intervals-in-row-n n (cdr sensors) intervals))
	  ;; O/w add this interval 
	   (T (intervals-in-row-n
	         n
	         (cdr sensors)
		 (append intervals (list (list (- (caar sensor) free-moves) (+ (caar sensor) free-moves))))))))))

(defun beacons-in-row-n (n beacons count)
  (cond ((not beacons) count)
	((equalp n (second (car beacons)))
	  (beacons-in-row-n n (cdr beacons) (+ count 1)))
	(T (beacons-in-row-n n (cdr beacons) count))))

(defun sum-intervals (intervals) (reduce #'+ (mapcar (lambda (x) (+ 1 (- (second x) (first x)))) intervals)))

;; 1
(time (print
 (- (sum-intervals (reduce-intervals (intervals-in-row-n 2000000 *sensors* NIL) NIL))
    (beacons-in-row-n 2000000 *beacons* 0))))


;; 2

(defun empty-space? (intervals min-look max-look)
  ;; A and b are intervals we look for a gap between
  (let ((a (first intervals)) (b (second intervals)))
    (cond ((not b) NIL)
	  ;; Inefficient to search if b is more than max look but nbd
	  ((or (< (second a) min-look) (> (first b) max-look))
	   (empty-space? (cdr intervals) min-look max-look))
	  ((> (- (first b) (second a)) 1) (+ (second a) 1))
	  (T (empty-space? (cdr intervals) min-look max-look)))))


(time (let ((res (loop for row from 0 to 4000000
      do (let ((rowints
		 (sort (reduce-intervals (intervals-in-row-n row *sensors* NIL) NIL)
			 #'< :key #'first)))


	   (let ((spot (empty-space? rowints 0 4000000)))
	     (if spot (return (list spot row)) NIL))))))
  (progn (print res) (print (+ (* (first res) 4000000) (second res))))))

