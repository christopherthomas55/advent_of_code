;; Commenting heavily cause I barely can understand lisp
;; "Cheating" and using a package to split strings by work
(ql:quickload :str)


;; Checks if string matches first in list. Trying to get comfy with car, cdr, cons, etc
(defun alist_equalp (str el_alist)
  (equalp str (CAR el_alist))
)

;; Gets val from assocation list of string using alist_equalp
;; Note that cdr works on association list cause it is a cons, not a list
(defun val_map (alist str)
 (CDAR (member str alist :test #'alist_equalp))
)


(defun win_total (beats_alist str-list)
  (let ((them (nth 0 str-list))(us (nth 1 str-list)))
    (if (equalp us (val_map (list (cons "A" "X") (cons "B" "Y") (CONS "C" "Z")) them)) 3
      ;; else
      (if (equalp them (val_map beats_alist us)) 6
      ;; else
	0
      )
    )

  )
)
;;(def parameter 
(defun round_result
 (string)
  (let ((str-list (str:words string))
	(val_alist (list (cons "X" 1) (cons "Y" 2) (CONS "Z" 3)))
	(beats_alist (list (cons "X" "C") (cons "Y" "A") (CONS "Z" "B")))
	)
    (+ (win_total beats_alist str-list)
       (val_map val_alist (nth 1 str-list))
    )
  )
)

;; Answer 1
(print
  (reduce #'+ 
    (mapcar #'round_result (uiop:read-file-lines "2.txt"))
  )
)

(defun choice_total (beats_alist loses_alist val_alist str-list)
  (let ((them (nth 0 str-list))(us (nth 1 str-list)))
    ;; They win so find what beats
    (if (equalp us "X") (val_map val_alist (val_map beats_alist them))
      ;; elsewe tie
      (if (equalp us "Y") (val_map val_alist them)
      ;; finally we win so beat
	  (val_map val_alist (val_map loses_alist them))
      )
    )

  )
)
(defun round_result2
 (string)
  (let ((str-list (str:words string))
	(result_alist (list (cons "X" 0) (cons "Y" 3) (CONS "Z" 6)))
	(val_alist (list (cons "A" 1) (cons "B" 2) (CONS "C" 3)))
	(beats_alist (list (cons "A" "C") (cons "B" "A") (CONS "C" "B")))
	;; Lazy and including losing too
	(loses_alist (list (cons "C" "A") (cons "A" "B") (CONS "B" "C")))
	)
    (+ (choice_total beats_alist loses_alist val_alist str-list)
       (val_map result_alist (nth 1 str-list))
    )
  )
)
;; Answer 2 

(print
  (reduce #'+ 
    (mapcar #'round_result2 (uiop:read-file-lines "2.txt"))
  )
)
