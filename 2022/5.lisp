(ql:quickload :str)

(defmacro position-data () `(uiop:read-file-lines "5a.txt"))
(defmacro moveset-data () `(uiop:read-file-lines "5b.txt"))


;;Since we push el we must reverse at end
(defun ingest-crate-data(in-str)
  (loop with sublist for el across in-str for n from 0
    if (equalp (mod n 4) 1)
      do (push (list el) sublist)
    finally
      (return (reverse sublist))))


(defun ingest-move-data(in-str)
  (let ((word-list (str:words in-str)))
    (list (parse-integer (nth 1 word-list))
          (parse-integer (nth 3 word-list))
          (parse-integer (nth 5 word-list)))))

(defun crate-reduce (lista listb)
  (loop for x in lista
	for y in listb
	collect (append x y)))


(defun remove-space (list) 
  (remove #\SPACE list :test #'equalp))


(defun substitute-nth (val n list)
  (loop for i from 0 for j in list collect (if (= i n) val j)))


(defun operate-action (el positions)
  (let (
     (n (first el))
     (from_spot (- (second el) 1))
     (to_spot (- (third el) 1)))
   (if (<= n 0) positions
     ;;else
     (progn
       ;; Move to new spot
       (setf positions
	 (substitute-nth
	   (append (nth to_spot positions) (last (nth from_spot positions)))
	   to_spot
	   positions))
       ;; Remove from old
       (setf positions
	 (substitute-nth
	   (butlast (nth from_spot positions))
	   from_spot
	   positions))
       (operate-action (list (- n 1) (+ 1 from_spot) (+ 1 to_spot)) positions)))))


(defun main ()
  (let ((positions
          (mapcar #'remove-space
  	  (reduce #'crate-reduce
  	    ;; Positions is bottom up to make life easier
  	    (reverse (mapcar #'ingest-crate-data (position-data))))
  	   ))
       (actions
           (mapcar #'ingest-move-data (moveset-data)))
       )
    (progn (loop for el in actions
	    do (setf positions (operate-action el positions)))
	   positions)))

;; Problem 1 - ugly!
(print (coerce (mapcar #'car (mapcar #'last (main))) 'string))

;; Problem 2

(defun operate-action-2 (el positions)
  (let (
     (n (first el))
     (from_spot (- (second el) 1))
     (to_spot (- (third el) 1)))
     ;;else
     (progn
       ;; Move to new spot
       (setf positions
	 (substitute-nth
	   (append (nth to_spot positions) (last (nth from_spot positions) n))
	   to_spot
	   positions))
       ;; Remove from old
       (setf positions
	 (substitute-nth
	   (butlast (nth from_spot positions) n)
	   from_spot
	   positions))
       positions)))

;; Gotta be a better way but I need to eat
(defun main-2 ()
  (let ((positions
          (mapcar #'remove-space
  	  (reduce #'crate-reduce
  	    ;; Positions is bottom up to make life easier
  	    (reverse (mapcar #'ingest-crate-data (position-data))))
  	   ))
       (actions
           (mapcar #'ingest-move-data (moveset-data)))
       )
    (progn (loop for el in actions
	    do (setf positions (operate-action-2 el positions)))
	   positions)))

(print (coerce (mapcar #'car (mapcar #'last (main-2))) 'string))
