;; Idea
;; - Reduce the graph into a simpler one with only flow-able valves
;; - Then it becomes a variation of a dynamic programming problem. Need to check wikipedia for dijkstras lmao

(ql:quickload :str)

(defun data-conversion (x)
  (list (first x) (parse-integer (second x)) (str:split "," (third x))))

(defparameter *raw-input* 
  (mapcar #'data-conversion (mapcar #'str:words (uiop:read-file-lines "16.txt"))))

(print *raw-input*)

(defun node-name (graph) (first graph))
(defun node-val (graph) (second graph))
(defun node-others (graph) (third graph))

;; (nodename visited? highdistance)
(defun dijkstra-data-helper (node)
  (list (first node) NIL 10000))

(defun node-from-name (name set)
  (find-if (lambda (x) (string= name (node-name x))) set))


;; Inefficient dijkstras since needs to traverse every step - could use a hash map instead
(defun dijkstras-on-raw (name0 name1 graph)
  (let ((nodes (mapcar #'dijkstra-data-helper graph)))
    (let ((current-node (node-from-name name0 nodes)))
      (progn (setf (third current-node) 0)
       ;; Loop until we hit  destination. Note, assuming there;s not a loop. If there is we need to check if all possibilities are > 10000 "infinity"
       (loop while (not (equalp current-node (node-from-name name1 nodes)))
	  ;
	  do (let ((neighbors (third (node-from-name (node-name current-node) graph))))
	       (progn
	         ;; Update each neighbor distance
		 (loop for el in neighbors
		       if (< (+ 1 (third current-node)) (third (node-from-name el nodes)))
		         do (setf (third (node-from-name el nodes)) (+ 1 (third current-node))))
		 ;; Mark current as visited
		 (setf (second current-node) T)
		 ;; Make new current as node with lowest
		 (let ((min-node-val (reduce #'min (mapcar #'third (remove-if #'second nodes)))))
		   (let ((new-node-name (node-name (find-if (lambda (x) (equalp min-node-val (third x))) (remove-if #'second nodes)))))
		     (setf current-node (node-from-name new-node-name nodes))))))
	  finally
	     (return (third (node-from-name name1 nodes))))))))

;;(dijkstras-on-raw "AA" "BB" *raw-input*)

(defun simplify-graph (graph)
  (loop for el in graph
	if (or (> (node-val el) 0) (string= (node-name el) "AA"))
          collect (list (node-name el) (node-val el)
			(loop for el2 in graph
			      if (and (> (node-val el2) 0) (not (equalp el el2)))
				collect (list (node-name el2) (dijkstras-on-raw (node-name el) (node-name el2) *raw-input*))))))
			

(defparameter *graph-official* (simplify-graph *raw-input*))

(print *graph-official*)


