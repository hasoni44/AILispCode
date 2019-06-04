;;FUNCTIONS GIVEN BY THE PROFESSOR

(defun shuffle (lis)
  ;;"Shuffles a list.  Non-destructive.  O(length lis), so
  ;;pretty efficient.  Returns the shuffled version of the list."
  (let ((vec (apply #'vector lis)) bag (len (length lis)))
    (dotimes (x len)
      (let ((i (random (- len x))))
	(rotatef (svref vec i) (svref vec (- len x 1)))
	(push (svref vec (- len x 1)) bag)))
    bag))   ;; 65 s-expressions, by the way


(defparameter *verify* t)

;; hmmm, openmcl keeps signalling an error of a different kind
;; when I throw an error -- a bug in openmcl?  dunno...
(defun throw-error (str)
  (error (make-condition 'simple-error :format-control str)))

(defun verify-equal (funcname &rest matrices)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (unless (and
	     (apply #'= (mapcar #'length matrices))
	     (apply #'= (mapcar #'length (mapcar #'first matrices))))
      (throw-error (format t "In ~s, matrix dimensions not equal: ~s"
			   funcname
			   (mapcar #'(lambda (mat) (list (length mat) 'by (length (first mat))))
				   matrices))))))

(defun verify-multiplicable (matrix1 matrix2)
  ;; we presume they're rectangular -- else we're REALLY in trouble!
  (when *verify*
    (if (/= (length (first matrix1)) (length matrix2))
	(throw-error (format t "In multiply, matrix dimensions not valid: ~s"
			     (list (list (length matrix1) 'by (length (first matrix1)))
				   (list (length matrix2) 'by (length (first matrix2)))))))))


;; Basic Operations

(defun map-m (function &rest matrices)
  ;;"Maps function over elements in matrices, returning a new matrix"
  (apply #'verify-equal 'map-m  matrices)
  (apply #'mapcar #'(lambda (&rest vectors)       ;; for each matrix...
		      (apply #'mapcar #'(lambda (&rest elts)     ;; for each vector...
					  (apply function elts))
			     vectors)) 
	 matrices))   ;; pretty :-)

(defun transpose (matrix)
  ;;"Transposes a matrix"
  (apply #'mapcar #'list matrix))  ;; cool, no?

(defun make-matrix (i j func)
  ;;"Builds a matrix with i rows and j columns,
  ;; with each element initialized by calling (func)"
  (map-m func (make-list i :initial-element (make-list j :initial-element nil))))

(defun make-random-matrix (i j val)
  ;;"Builds a matrix with i rows and j columns,
  ;;  with each element initialized to a random
  ;;  floating-point number between -val and val"
  (make-matrix i j #'(lambda (x)
		       (declare (ignore x))  ;; quiets warnings about x not being used
		       (- (random (* 2.0 val)) val))))

(defun e (matrix i j)
  ;;"Returns the element at row i and column j in matrix"
  ;; 1-based, not zero-based.  This is because it's traditional
  ;; for the top-left element in a matrix to be element (1,1),
  ;; NOT (0,0).  Sorry about that.  :-)
  (elt (elt matrix (1- i)) (1- j)))

(defun print-matrix (matrix)
  ;;"Prints a matrix in a pleasing form, then returns matrix"
  (mapcar #'(lambda (vector) (format t "~%~{~8,4,,F~}" vector)) matrix) matrix)

;; Matrix Multiplication

(defun multiply2 (matrix1 matrix2)
  ;;"Multiplies matrix1 by matrix2 
  ;;  -- don't use this, use multiply instead"
  (verify-multiplicable matrix1 matrix2)
  (let ((tmatrix2 (transpose matrix2)))
    (mapcar #'(lambda (vector1)
		(mapcar #'(lambda (vector2)
			    (apply #'+ (mapcar #'* vector1 vector2))) tmatrix2))
	    matrix1)))  ;; pretty :-)

(defun multiply (matrix1 matrix2 &rest matrices)
  ;;"Multiplies matrices together"
  (reduce #'multiply2 (cons matrix1 (cons matrix2 matrices))))

;; Element-by-element operations

(defun add (matrix1 matrix2 &rest matrices)
  ;;"Adds matrices together, returning a new matrix"
  (apply #'verify-equal 'add matrix1 matrix2 matrices)
  (apply #'map-m #'+ matrix1 matrix2 matrices))

(defun e-multiply (matrix1 matrix2 &rest matrices)
  ;;"Multiplies corresponding elements in matrices together, 
  ;;      returning a new matrix"
  (apply #'verify-equal 'e-multiply matrix1 matrix2 matrices)
  (apply #'map-m #'* matrix1 matrix2 matrices))

(defun subtract (matrix1 matrix2 &rest matrices)
  ;;"Subtracts matrices from the first matrix, returning a new matrix."
  (let ((all (cons matrix1 (cons matrix2 matrices))))
    (apply #'verify-equal 'subtract all)
    (apply #'map-m #'- all)))

(defun scalar-add (scalar matrix)
  ;;"Adds scalar to each element in matrix, returning a new matrix"
  (map-m #'(lambda (elt) (+ scalar elt)) matrix))

(defun scalar-multiply (scalar matrix)
  ;;"Multiplies each element in matrix by scalar, returning a new matrix"
  (map-m #'(lambda (elt) (* scalar elt)) matrix))

;; This function could
;; be done trivially with (scalar-add scalar (scalar-multiply -1 matrix))
(defun subtract-from-scalar (scalar matrix)
  ;;"Subtracts each element in the matrix from scalar, returning a new matrix"
  (map-m #'(lambda (elt) (- scalar elt)) matrix))


;;;; Some useful preprocessing functions

(defun scale-list (lis)
  ;;"Scales a list so the minimum value is 0.1 and the maximum value is 0.9.  Don't use this function, it's just used by scale-data."
  (let ((min (reduce #'min lis))
        (max (reduce #'max lis)))
        (if (= min max) 
            (mapcar (lambda (elt) 0.1) lis)
            (mapcar (lambda (elt) (+ 0.1 (* 0.8 (/ (- elt min) (- max min)))))
                    lis))))

(defun scale-data (lis)
  ;;"Scales all the attributes in a list of samples of the form ((attributes) (outputs))"
  (transpose (list (transpose (mapcar #'scale-list (transpose (mapcar #'first lis))))
		   (transpose (mapcar #'scale-list (transpose (mapcar #'second lis)))))))

(defun convert-data (raw-data)
  ;;"Converts raw data into column-vector data of the form that
  ;;can be fed into NET-LEARN.  Also adds a bias unit of 0.5 to the input."
  (mapcar #'(lambda (datum)
	      (mapcar #'(lambda (vec)
			  (mapcar #'list vec))
		      (list (cons 0.5 (first datum))
			    (second datum))))
	  raw-data))

(defun average (lis)
  ;;"Computes the average over a list of numbers.  Returns 0 if the list length is 0."
  (if (= (length lis) 0)
      0
      (/ (reduce #'+ lis) (length lis))))

;;;-----------------------------------------------------------------------------

;;functions that need to be implemented:

;;SIGMOID

(defun sigmoid (u)
	
  ;;"Sigmoid function applied to the number u"
  ;;S(u) = 1 / (1 + e^-u) = e^u / e^u + 1
  (/ (exp u) (+ (exp u) 1)))

;;------------------------------------------------------------------------------

;;NET-ERROR
;; output and correct-output are both column-vectors


(defun net-error (output correct-output)
  ;;Returns (as a scalar value) the error between the output and correct vectors
  ;;error =  0.5 ( tr[c - o] . (c - o) )
  ;;tr = transpose
  ;;c - o <- subtracting two matricies
  ;; . = dot product 
  ;;multiply by 0.5 in the end
  ;;        |------------------------MATRIX, only 1x1 extract the value from it-------------------|
  (* 0.5 (e (multiply (transpose (subtract correct-output output)) (subtract correct-output output)) 1 1)))


;;------------------------------------------------------------------------------

;;FORWARD-PROPAGATE

;; a single datum is of the form
;; (--input-column-vector--  -- output-column-vector--)
;;
;; Notice that this is different from the raw data provided in the problems below.
;; You can convert the raw data to this column-vector form using CONVERT-DATA

;;datum is always _ by 2

;;The forward pass rules are (in this order):



;;get input helper function
(defun get-input (datum)
	;;makes new list
	(let ((inputlist '()))
		(loop for i in datum
			;;just appends first col of every row to the new list
			do (setq inputlist (append inputlist (list (nth 0 i))))
	finally (return inputlist))))

;;get output helper function, similar to get input function except takes second col
(defun get-output (datum)
	(let ((inputlist '()))
		(loop for i in datum
			do (setq inputlist (append inputlist (list (nth 1 i))))
	finally (return inputlist))))

(defun forward-propagate (datum v w)
	
  ;;h = sigmoid[v . i]
  ;;o = sigmoid[w . h]
  
  (let ((h (map-m sigmoid (multiply v (get-input datum))))) ;; complete the h operation here
  	  (map-m sigmoid (multiply w h)))) ;; complete the o operation here and it will be returned

;;------------------------------------------------------------------------------
;;BACK-PROPAGATE

	;;The backpropagation rules are (in this order):
	
	;;h = sigmoid[v . i]
    ;;o = sigmoid[w . h]
    
    ;;o = output (use o equation above)
    ;;c = correct-output

    ;; (mulitply (mulitply (subtract c o) o) (subtract-from-scalar 1 o))
	;;odelta = (c - o) o (1 - o)
	
	;;(multiply h (multiply (subtract 1 h) (multiply (transpose w) odelta)))
	;;hdelta = (h (1 - h) (tr[w] . odelta))
	
	;;(add w (multiply alpha (multiply odelta (transpose h))))
	;;w = w + alpha (odelta . tr[h])
	
	;;(add v (multiply alpha (multiply hdelta (transpose (get-input datum)))))
	;;v = v + alpha (hdelta . tr[i])
	
	
(defun back-propagate (datum alpha v w)
	
  ;;"Back-propagates a datum through the V and W matrices,
  ;;returning a list consisting of new, modified V and W matrices."
  ;; Consider using let*
  ;; let* is like let, except that it lets you initialize local
  ;; variables in the context of earlier local variables in the
  ;; same let* statement.
  
  (let ((h (map-m sigmoid (multiply v (get-input datum)))) ;; create h
  		(o (map-m sigmoid (multiply w h))) ;; create o
  		(odelta (mulitply (mulitply (subtract (get-output datum) o) o) (subtract-from-scalar 1 o))) ;; create odelta
  		(hdelta (multiply h (multiply (subtract 1 h) (multiply (transpose w) odelta)))) ;; create hdelta
  		(w (add w (multiply alpha (multiply odelta (transpose h))))) ;; create w 
  		(v (add v (multiply alpha (multiply hdelta (transpose (get-input datum))))))) ;; complete v here
  	  (list v w)) ;; eventually return a list of v and w that were modified
  
  )	


;;------------------------------------------------------------------------------
;;NET-BUILD




(defun optionally-print (x option)
  ;;"If option is t, then prints x, else doesn't print it.
  ;;In any case, returns x"
  ;;; perhaps this might be a useful function for you
  (if option (print x) x))


(defparameter *a-good-minimum-error* 1.0e-9)



;; data is of the form:
;; (
;;  (--input-column-vector--  --output-column-vector--)
;;  (--input-column-vector--  --output-column-vector--)
;;  ...
;; )
;;
;;
;; Notice that this is different from the raw data provided in the problems below.
;; You can convert the raw data to this column-vector form using CONVERT-DATA


;;; IMPLEMENT THIS FUNCTION

(defun net-build (data num-hidden-units alpha initial-bounds max-iterations modulo &optional print-all-errors)
	
;;"Builds a neural network with num-hidden-units and the appropriate number
;;of input and output units based on the data.  Each element should be a random
;;value between -(INITIAL-BOUNDS) and +(INITIAL-BOUNDS).

;;Then performs the following loop MAX-ITERATIONS times, or until the error condition
;;is met (see below):

;;   1. For each data element in a randomized version of the data, perform
;;      backpropagation.
;;   2. Every modulo iterations,
;;          For every data element in the data, perform forward propagation and
;;          A.  If print-all-errors is true, then print the error for each element
;;          B.  At any rate, always print the worst error and the mean error
;;          C.  If the worst error is better (lower) than A-GOOD-MINIMUM-ERROR,
;;              quit all loops and prepare to exit the function --
;;              the error condition was met.

;;The function should return a list of two items: the final V matrix
;;and the final W matrix of the learned network."
  )


;;SIMPLE-GENERALIZATION
;;K-FOLD-VALIDATION

;; For this function, you should pass in the data just like it's defined
;; in the example problems below (that is, not in the "column vector" format
;; used by NET-BUILD.  Of course, if you need to call NET_BUILD from this function
;; you can alway convert this data to column-vector format using CONVERT-DATA within
;; the SIMPLE-GENERALIZATION function.
;;
;; Yes, this is ridiculously inconsistent.  Deal with it.  :-)

;;; IMPLEMENT THIS FUNCTION

(defun simple-generalization (data num-hidden-units alpha initial-bounds max-iterations)

  )



;; For this function, you should pass in the data just like it's defined
;; in the example problems below (that is, not in the "column vector" format
;; used by NET-BUILD.  Of course, if you need to call NET_BUILD from this function
;; you can alway convert this data to column-vector format using CONVERT-DATA within
;; the SIMPLE-GENERALIZATION function.
;;
;; Yes, this is ridiculously inconsistent.  Deal with it.  :-)


(defun k-fold-validation (data k num-hidden-units alpha initial-bounds max-iterations)
 )




;;;; Some useful preprocessing functions

(defun scale-list (lis)
  ;;"Scales a list so the minimum value is 0.1 and the maximum value is 0.9.  Don't use this function, it's just used by scale-data."
  (let ((min (reduce #'min lis))
	(max (reduce #'max lis)))
    (mapcar (lambda (elt) (+ 0.1 (* 0.8 (/ (- elt min) (- max min)))))
	    lis)))

(defun scale-data (lis)
  ;;"Scales all the attributes in a list of samples of the form ((attributes) (outputs))"
  (transpose (list (transpose (mapcar #'scale-list (transpose (mapcar #'first lis))))
		   (transpose (mapcar #'scale-list (transpose (mapcar #'second lis)))))))

(defun convert-data (raw-data)
  ;;"Converts raw data into column-vector data of the form that
  ;;can be fed into NET-LEARN.  Also adds a bias unit of 0.5 to the input."
  (mapcar #'(lambda (datum)
	      (mapcar #'(lambda (vec)
			  (mapcar #'list vec))
		      (list (cons 0.5 (first datum))
			    (second datum))))
	  raw-data))

(defun average (lis)
  ;;"Computes the average over a list of numbers.  Returns 0 if the list length is 0."
  (if (= (length lis) 0)
      0
      (/ (reduce #'+ lis) (length lis))))


;;; here are the inputs and outputs of your three problems to test
;;; the net function on.


