(ql:quickload :str)

;; SOme empty strings show up around [] but that's fine
(defun str-handler (string)
  (loop for el across string
	with out = "" and matched = NIL
	if (or (char= el #\[) (char= el #\]))
	  collect out and collect (string el) and do (setf out "") (setf matched T)
	if (char= el #\,)
	  collect out and do (setf out "") (setf matched T)
	if (not matched)
          do (setf out (str:concat out (string el)))
	else do (setf matched NIL)))
	  
	
(defun str-to-lists (str)
  (loop for el in (str-handler str)
	with sublist_stack = NIL
	do (cond ((string= el "[") (push (list) sublist_stack))
                  ((string= el ",") NIL)
                  ((string= el "") NIL)
		  ;; Weird operation needs testing - TESTED
                  ((and (string= el "]") (not (equalp 1 (length sublist_stack))))
		   (let ((x (pop sublist_stack)))
		     (setf (first sublist_stack) (append (first sublist_stack) (list x)))))
                  ((string= el "]") NIL)
		  ;; Else assuming is integer for speed, not robust
		  ( T (setf (first sublist_stack) (append (first sublist_stack) (list (parse-integer el)))))) 
		  


	finally (return (first sublist_stack))))


;; Data confirmed working on test input
(defun data ()
  (loop for el in (uiop:read-file-lines "13.txt")
        for count from 1
  with sublist = (list)
  if (zerop (mod count 3))
    collect sublist and do (setf sublist (list))
  else
    do (setf sublist (append sublist (list (str-to-lists el))))))

;; a b are ints or lists. This is hacky and ugly
(defun el-compare (a b)
  (progn ;;(print a) (print b) (print "---")
    (cond
      ;; a null, b not is great
      ((and (not a) (not (not b))) 1)
      ;; b null, a not is not great
      ((and (not b) (not (not a))) -1)
      ;; both is nothing
      ((and (not b) (not a)) 0)

      ;; Both numbers, lower integer should be on left. If equal cont
      ((and (numberp a) (numberp b))
         (cond ((< a b) 1)
	       ((equalp a b) 0)
	       ((> a b) -1)))
      ;; Both lists? Recursive check if smaller. If both are same length continue, o/w stop
      ((and (listp a) (listp b))
       (let ((compare (el-compare (first a) (first b)))
	     (both_null (and (not (first a)) (not (first b)))))
	 (cond ((and (not (not compare)) (not (zerop compare))) compare)
	        (both_null 0)
	        (T (el-compare (cdr a) (cdr b))))))
      ;; Left is number and right is list? Left gotta be smaller and continue
      ((and (numberp a) (listp b))
         (el-compare (list a) b))
      ;; Left is list and right is number? Left better be size 1, smaller and continue
      ((and (listp a) (numberp b))
         (el-compare a (list b)))
      ;; If this is the case the just give up
      (T 0))))

;; T if less than. O/w fals
(defun comparison-wrapper (x)
  (loop for el in (first x)
        for el2 in (second x)
	for i from 0
        if (equalp 1 (el-compare el el2))
	  do (return T)
        if (equalp -1 (el-compare el el2))
	  do (return NIL)
	;; only loops over same number idx. Here we check to make sure el is first x  
	finally (if (and (not (nth (+ i 1) (second x))) (nth (+ i 1) (first x))) (return NIL) (return T))))
	
;; 1 logic
(reduce #'+ (loop for el in (mapcar #'comparison-wrapper (data))
      for idx from 1
      if el
	collect idx))

;; 2
(defun compare-lists (a b)
  (loop for el in a
        for el2 in b
	for i from 0
        if (equalp 1 (el-compare el el2))
	  do (return T)
        if (equalp -1 (el-compare el el2))
	  do (return NIL)
	;; only loops over same number idx. Here we check to make sure el is first x  
	finally (if (and (not (nth (+ i 1) b)) (nth (+ i 1) a)) (return NIL) (return T))))

(defun smaller-2 (x) (compare-lists x '((2))))
(defun smaller-6 (x) (compare-lists x '((6))))

(defun count-t (list) (loop for x in list with count = 0 if x do (setf count (+ count 1)) finally (return count)))
;; flattened data. And count smaller than 2/6. Add 1 to six since 2 is supposed to be here too
(* (count-t (mapcar #'smaller-2 (loop for el in (data) collect (first el) collect (second el))))
   (+ 1 (count-t (mapcar #'smaller-6 (loop for el in (data) collect (first el) collect (second el))))))
