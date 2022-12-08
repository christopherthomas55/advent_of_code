;; Defining dirobject structure as
;;(name (file1 file2 file3)
;;  (dirobject1 dirobject2 ...))

;; So dir size is defined by the recursive relationship
;; (dir-size (dir)
;;   If no subdirectories (stored in third entry
;;   (if (not (third dir))
;;     (reduce #'+ (second dir))
;;     (+ (reduce #'+ (second dir)) (mapcar #'dir-size (third dir))))

(ql:quickload :str)

(defmacro command-data () `(uiop:read-file-lines "7.txt"))

;; Contains filesystem in tree structure above
(defparameter *filesystem* (list "/" '() '()))
;; Contains list of cds to get to spot
(defparameter *current-spot* ())

;; For dir check
(defun first-equal (list check)
  (loop for subdir in list
    when (string= (car subdir) check)
      return subdir))

(defun traverse-to-spot (spot filesystem)
  (if (not spot)
    filesystem
    (traverse-to-spot (cdr spot) (first-equal (third filesystem) (car spot)))))

(defun add-directory (name)
  (setf (third (traverse-to-spot *current-spot* *filesystem*))
	(append (third (traverse-to-spot *current-spot* *filesystem*))
		;;Gotta make the appended dir that weird structure
		(list (list name (list) (list))))))

(defun add-file (size)
  (setf (second (traverse-to-spot *current-spot* *filesystem*))
	(append (second (traverse-to-spot *current-spot* *filesystem*)) (list size))))

(defun handle-command (strlist) 
  (let ((cmd (second strlist)))
    (if (string= "cd" cmd)
      (if (string= ".." (third strlist))
	(setf *current-spot* (butlast *current-spot* 1))
        (setf *current-spot* (append *current-spot* (list (third strlist))))))))


(defun handle-content (strlist)
  (if (equalp (char (first strlist) 0) #\d)
    (add-directory (second strlist))
    (add-file (parse-integer (first strlist)))))

;; Create filesystem data structure
(loop for el in (command-data)
  do (if (equalp (char el 0) #\$)
         ;; Starts with $ indicates command
         (handle-command (str:words el))
	 ;; Otherwise we
	 (handle-content (str:words el))))


;;I can't believe the above worked first try. Time to sum that

(defparameter size-list ())

(defun dir-size (dir)
  (if (not (third dir))
    (reduce #'+ (second dir))
    (+ (reduce #'+ (second dir)) (reduce #'+ (mapcar #'dir-size (third dir))))))

;; Main function to calc size of each dir and store in size-list. Top down recursion here terribly inefficient
(defun check-dir-size (dir)
  (progn 
    (setf size-list (append size-list (list (dir-size dir))))
    (if (not (not (third dir)))
        (mapcar #'check-dir-size (third dir))
	())))

;; It's sad I can't figure out how to filter a list yet so I am resorting to makging a function to do that
(defun too-big (n) (if (> n 100000) 0 n))

;; Problem 1
(check-dir-size *filesystem*)
(print size-list)
(print (reduce #'+ (mapcar #'too-big size-list)))

(defun intmax (a b)(if (> a b) a b))
(defun intmin (a b)(if (< a b) a b))

;; Problem 2
(print (let ((need-at-least (- 30000000 (- 70000000 (reduce #'intmax size-list)))))
   (progn (defun too-small (n) (if (< n need-at-least) 70000000 n))
           (reduce #'intmin (mapcar #'too-small size-list)))))
