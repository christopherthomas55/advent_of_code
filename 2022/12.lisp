(defmacro data () `(uiop:read-file-lines "12.txt"))


;;a-z = 97 - 122
(defun char-val (char)
  (if (char= #\E char)
    (char-code #\z)
    (if (char= #\S char)
     (char-code #\a)
     (char-code char))))

(defun find-char (char nested_list)
  (loop for el in nested_list
	for y from 0
	if (search char el)
	  collect (list (search char el) y)))


; Goal locations swapped since it's easier to start at end
(defun prep-data ()
  (let ((in-data (data)))
  (progn (defparameter *ysize* (length in-data))
	 (defparameter *xsize* (length (first in-data)))
	 (defparameter *goal* (first (find-char "S" in-data)))
	 (defparameter *start* (first (find-char "E" in-data)))
	 (defparameter *heightdata* (mapcar (lambda (x) (coerce x 'list)) in-data))
	 ;; Initial element is extra high. Have to loop here cause inital element has shared list
	 (defparameter *visited*
	   (loop for j below *ysize*
	      collect (loop for i below *xsize* collect 100000))))))

(defun in-grid (location)
  (and (< (first location) *xsize*)
       (>= (first location) 0)
       (< (second location) *ysize*)
       (>= (second location) 0)))

(defun add-coords (a b)
  (list (+ (first a) (first b))
	(+ (second a) (second b))))

(defun set-visited (location val)
  (setf (nth (first location) (nth (second location) *visited*)) val))

(defun is-closest-visited (location val)
  (> (nth (first location) (nth (second location) *visited*)) val))

(defun height-at-location (location) (nth (first location) (nth (second location) *heightdata*)))
(defun distance-at-location (location) (nth (first location) (nth (second location) *visited*)))
(defun can-move-to (from to)
  (>= 1 (- (char-val (height-at-location to)) (char-val (height-at-location from)))))


(defun search-path (location distance)
  (let ((nearby (list (add-coords location '(1 0))
		      (add-coords location '(-1 0))
		      (add-coords location '(0 1))
		      (add-coords location '(0 -1)))))
    (loop for direction in nearby
	  do (if (and (in-grid direction)
		  (can-move-to direction location)
		  (is-closest-visited direction (+ distance 1)))
        (progn (set-visited direction (+ 1 distance))
               (search-path direction (+ distance 1)))
        ))))

(prep-data)
(set-visited *start* 0)
(search-path *start* 0)
;; Part 1
(print (distance-at-location *goal*))

;; Part 2 just need to find min where is a
(defparameter *min* 440)
(loop for el in *visited*
      for y from 0
      do (loop for el2 in el
	       for x from 0
	       do (if (and (char= #\a (height-at-location (list x y)))
			   (< (distance-at-location (list x y)) *min*))
		    (setf *min* (distance-at-location (list x y)))
		    NIL)))
(print *min*)

