;;DIFFERENCE
;;gives difference between two lists
;;three kinds of measurements for difference:
	;;COUNT <-- DEFAULT
	;;SQUARED
	;;MANHATTAN

;;called as a helper funciton
(defun counting (list1 list2)
(let ((counter 0))
;;go through both lists
(loop for i in list1
for j in list2 do
;;anytime there is an inequality, increment the counter
(if (equalp i j) (+ counter 0) (incf counter))
;;return the counter in the end
finally (return counter))))

;;called as a helper funciton
(defun squared (list1 list2)
(let ((counter 0))
;;go through both lists
(loop for i in list1
for j in list2 do
;;completes the squared operation on each pair of numbers and adds result to counter
(incf counter (* (abs (- i j)) (abs (- i j))))
;;returns the counter when loop completes
finally (return counter))))

;;called as a helper funciton
(defun manhattan (list1 list2)
(let ((counter 0))
;;go through both lists
(loop for i in list1
for j in list2 do
;;completes the manhattan operation on each pair of numbers and adds result to counter
(incf counter (abs (- i j)))
;;returns the counter when loop completes
finally (return counter))))

;;the main function
(defun difference (list1 list2 &key (measure 'count))
(let ((counter 0)) 
;;calls helper functions based on the key value
  (cond ((eql measure 'count) (counting list1 list2)) 
        ((eql measure 'squared) (squared list1 list2))
        ((eql measure 'manhattan) (manhattan list1 list2)))))

;;------------------------------------------------------------------------------

;;I am providing this function for you as an example
;;EXAMPLE GIVEN BY PROFESSOR
(defun most-common (list)
(let ((reduced-elts (mapcar (lambda (elt) (list elt (count elt list)))
(remove-duplicates list))))
(first (first (sort reduced-elts #'> :key #'second)))))

;;random_notes:
;;(append new-dif-list (list v))
;;(nconc v u)
;;(nth 1 i)
;;(difference (car i) new-example measure)

;;differences list
(defun list-of-differences (examples new-example &key (measure 'count))
;;create a new list to add on to
(let ((new-dif-list '()))
;;loop through the examples
(loop for i in examples
;;will return a new list in the form of ((difference class) (difference class)...)
do (setq new-dif-list (append new-dif-list (list (push (difference (car i) new-example :measure measure) (nth 1 i)))))
finally (return new-dif-list))))


;;takes in differences list, produces list of classes to be sent to most-common
(defun grab-and-final (differences-list numK)
;;sorts the input list, creates a new list to be added to
(let ((sorted-input (sort (copy-list differences-list)  #'< :key #'first)))
;;loop through from 0 to k-1, grab all the classes and return them in a list
(loop for i from 0 to (- numK 1)
for answer = (nth 1 (nth i sorted-input))
collect answer)))

;;K-NEAREST-NEIGHBOR
(defun k-nearest-neighbor (examples new-example &key (k 1) (measure 'count))
;;calls the previous helper functions and completely relies on them
(list (most-common (grab-and-final (list-of-differences examples new-example :measure measure) k))))

;;------------------------------------------------------------------------------

;;GENERALIZATION

(defun generalization (training-examples test-examples &key (k 1) (measure 'count))
;;start a counter for correct and incorrect
(let ((correct 0) (incorrect 0))
;;go through training and test
(loop for i in test-examples do
;;find K-NN of each data block in test based on training set
(if (equalp (k-nearest-neighbor training-examples (nth 0 i) :k k :measure measure) (nth 1 i)) (incf correct) (incf incorrect))
;;if K-NN == test class block -> correct++ else incorrect++
;;finally return (correct / correct+incorrect)
finally (return (/ correct (+ correct incorrect))))))
