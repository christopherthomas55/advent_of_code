(defun sum-sublist (l)
  (reduce #'+ l)
)

(defun sum-elves (input)
  (mapcar #'sum-sublist
  ;; Returns sublist 
  ;; https://www.reddit.com/r/lisp/comments/m5grm5/split_list_into_sublists/
    (loop with sublist for el in input
	if(equal el "")
	  collect sublist and do (setf sublist nil)
	else
	  do (push (parse-integer el) sublist)
     )
  )
)

;; Answer 1
(print (first (sort (sum-elves (uiop:read-file-lines "1.txt")) #'>)))

(defun sum-n (n input-list) 
  (sum-sublist (nthcdr (- (length input-list) n) (reverse input-list)))
)

;; Answer 2
(print (sum-n 3 (sort (sum-elves (uiop:read-file-lines "1.txt")) #'>)))
