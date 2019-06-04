;;FUNCTIONS GIVEN BY PROFESSOR

;;; Some utility Functions and Macros that you might find to be useful (hint)

(defmacro while (test &rest body)
  ;;"Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

;;; Example usage
;;;
;;; (let ((x 0))
;;;    (while (< x 5)
;;;        (print x)
;;;        (incf x)))


(defun random? (&optional (prob 0.5))
  ;;"Tosses a coin of prob probability of coming up heads,
  ;;then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  ;;"Generates a list of size NUM, with each element created by
  ;;(funcall FUNCTION).  If no-duplicates is t, then no duplicates
  ;;are permitted (FUNCTION is repeatedly called until a unique
  ;;new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))

;; hope this works right
(defun gaussian-random (mean variance)
  ;;"Generates a random number under a gaussian distribution with the
  ;;given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


;;; FITNESS EVALUATION FUNCTIONS

;;; I'm providing you with some classic objective functions.  See section 11.2.2 of
;;; Essentials of Metaheuristics for details on these functions.
;;;
;;; Many of these functions (sphere, rosenbrock, rastrigin, schwefel) are
;;; traditionally minimized rather than maximized.  We're assuming that higher
;;; values are "fitter" in this class, so I have taken the liberty of converting
;;; all the minimization functions into maximization functions by negating their
;;; outputs.  This means that you'll see a lot of negative values and that's fine;
;;; just remember that higher is always better.
;;; 
;;; These functions also traditionally operate with different bounds on the
;;; minimum and maximum values of the numbers in the individuals' vectors.  
;;; Let's assume that for all of these functions, these values can legally
;;; range from -5.12 to 5.12 inclusive.  One function (schwefel) normally goes from
;;; about -511 to +512, so if you look at the code you can see I'm multiplying
;;; the values by 100 to properly scale it so it now uses -5.12 to 5.12.


(defun sum-f (ind)
  ;;"Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  ;;"Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  ;;"Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  ;;"Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  ;;"Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  ;;"Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))

;;;-----------------------------------------------------------------------------
;;;;;; TOP-LEVEL EVOLUTIONARY COMPUTATION FUNCTIONS 

;;; TOURNAMENT SELECTION

;; is this a good setting?  Try tweaking it (any integer >= 2) and see

;;(random NUM) -> produced random positive number between 0 and num

;;; See Algorithm 32 of Essentials of Metaheuristics
(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  ;;; "Does one tournament selection and returns the selected individual."
  ;;pick a random number within the range of population
   (setq randomnum (random (- (list-length population) 1))) 
  	   ;;find the best fitness based on this index
  	   (setq bestfit (nth randomnum fitnesses))
  	   ;;find the best individual based on this index
  	   (setq best (nth randomnum population))
  	  ;;go through the number of the tournament
   	   (loop for x from 2 to *tournament-size*
   	   	   ;;find a new random space to pick from
   	   	   do (setq randomnum (random (- (list-length population) 1)))
   	   	   ;;set the next and the next fitness
   	   	   do (setq next (nth randomnum population))
   	   	   do (setq nextfit (nth randomnum fitnesses))
   	   	   
   	   	   ;;if the next beats the best, make it the new best
   	   	   if (> nextfit bestfit)
   	   	   do (setq bestfit nextfit)
   	   	   do (setq best next)
   	   	   ;;RETURN BEST HERE
   	   	   finally (return best)))

;;------------------------------------------------------------------------------

(defun tournament-selector (num population fitnesses)
  ;;"Does NUM tournament selections, and puts them all in a list, then returns the list"

  ;;; IMPLEMENT ME
  ;;;
  ;;; Hint: This is a very short function.  Maybe one of the
  ;;; Utility functions I provided might be of benefit here

  ;;; does tornament-select-one NUM amount of times and adds result to list
  ;;; Example usage
  
  ;;set x 
  (let ((x 0)
  ;;make a new empty list
       (tourlist '()))
  (loop for x from 1 to num
  	  do (setq tourlist (append tourlist (list (tournament-select-one population fitnesses))))
  finally (return tourlist))))

;;;A PROVIDED FUNCTION
;; I'm nice and am providing this for you.  :-)
(defun simple-printer (pop fitnesses)
  ;;"Determines the individual in pop with the best (highest) fitness, then
  ;;prints that fitness and individual in a pleasing manner."
  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))


;;------------------------------------------------------------------------------

(defun evolve (generations pop-size &key setup creator selector modifier evaluator printer)
  ;;"Evolves for some number of GENERATIONS, creating a population of size
  ;;POP-SIZE, using various functions"

  ;;; IMPLEMENT ME
  ;;;
  ;; The functions passed in are as follows:
  ;;(SETUP)                     called at the beginning of evolution, to set up
  ;;                            global variables as necessary
  ;;(CREATOR)                   creates a random individual
  ;;(SELECTOR num pop fitneses) given a population and a list of corresponding fitnesses,
  ;;                            selects and returns NUM individuals as a list.
  ;;                            An individual may appear more than once in the list.
  ;;(MODIFIER ind1 ind2)        modifies individuals ind1 and ind2 by crossing them
  ;;                            over and mutating them.  Returns the two children
  ;;                            as a list: (child1 child2).  Nondestructive to
  ;;                            ind1 and ind2.
  ;;(PRINTER pop fitnesses)     prints the best individual in the population, plus
  ;;                            its fitness, and any other interesting statistics
  ;;                            you think interesting for that generation.
  ;;(EVALUATOR individual)      evaluates an individual, and returns its fitness.
  ;;Pop will be guaranteed to be a multiple of 2 in size.
  ;;
  ;; HIGHER FITNESSES ARE BETTER

  ;; your function should call PRINTER each generation, and also print out or the
  ;; best individual discovered over the whole run at the end, plus its fitness
  ;; and any other statistics you think might be nifty.

  ;;; HINTS: You could do this in many ways.  But I implemented it using
  ;;; the following functions (among others)
  ;;;
  ;;; FUNCALL FORMAT MAPCAR LAMBDA APPLY
  ;;; (simple-printer population fitnesses)
  ;;; tournament-selector (num population fitnesses)
  
  
  ;;i just the setup function first
  (funcall setup)
  
  ;;create a new individual
  (creator)
  
  ;;print the best in the generation
  (simple-printer generation pop-size)
)

;;;;----------------------------------------------------------------------------

;;;;;; FLOATING-POINT VECTOR GENETIC ALGORTITHM


;;; Here you will implement creator, modifier, and setup functions for
;;; individuals in the form of lists of floating-point values.  
;;; I have provided some objective functions which you can use as
;;; fitness evaluation functions.

;;; If you were really into this, you might try implementing an evolution
;;; strategy instead of a genetic algorithm and compare the two.
;;;
;;; If you were really REALLY into this, I have an extension of this
;;; project which also does genetic programming as well.  That is a much
;;; MUCH MUCH MUCH more difficult project.



(defparameter *float-vector-length* 20 
  "The length of the vector individuals")
(defparameter *float-min* -5.12 
  "The minimum legal value of a number in a vector") 
(defparameter *float-max* 5.12 
  "The maximum legal value of a number in a vector")

;;HELPER FUNCTION TO CREATE A 1X1 LIST OF A RANDOM FLOAT WITHIN RANGE
(defun float-list-creator()
	
	;;(random 2) -> generates either a 1 or 0 randomly
	;;get either 1 or 0 and put that to the power of 1
	;;this determines if the random number is negative or positive
	
	;;use the random function to get a random number between 0 and 
	;;the float max, and then multiply by the sign
	
	;;return that number in a list
	
	;;(let (pos (expt -1 (random 2)))
	;;	 (ran (random *float-max*))
	;;	 (final (list (* ran pos)))))

    (list (* (random *float-max*) (expt -1 (random 2)))))

(defun float-vector-creator ()
  ;;"Creates a floating-point-vector *float-vector-length* in size, filled with
  ;;UNIFORM random numbers in the range appropriate to the given problem"

  ;;; IMPLEMENT ME
  ;;;
  ;;; The numbers must be uniformly randomly chosen between *float-min* and
  ;;; *float-max*.  See the documentation for the RANDOM function.

  ;;; HINT: Maybe a function I provided in the utilities might
  ;;; be handy here
  
  ;;*float-vector-length*
  ;;create new list for vectors to be stored in
  (let ((vectorlist '()))
  	  ;;for the needed length
		(loop for i from 1 to *float-vector-length*
			;;continue to add a new random list/element of vector using helper function
			do (setq vectorlist (append vectorlist (list (float-list-creator))))
    ;;return the final vector list
	finally (return vectorlist))))

;; I just made up these numbers, you'll probably need to tweak them
(defparameter *crossover-probability* 0.1
  "Per-gene probability of crossover in uniform crossover")
(defparameter *mutation-probability* 0.1
  "Per-gene probability of mutation in gaussian convolution") 
(defparameter *mutation-variance* 0.02
  "Per-gene mutation variance in gaussian convolution")

;; to implement FLOAT-VECTOR-MODIFIER, the following two functions are
;; strongly reccommended.

(defun uniform-crossover (ind1 ind2)
  ;;"Performs uniform crossover on the two individuals, modifying them in place.
  ;;*crossover-probability* is the probability that any given allele will crossover.  
  ;;The individuals are guaranteed to be the same length.  Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For crossover: use uniform crossover (Algorithm 25) in
  ;;;                Essentials of Metaheuristics
  ;;; HINTS:
  ;;; DOTIMES, ELT, and ROTATEF
  
  ;;(random 1.1) -> produces random number between 0 and 1.0 inclusively
  
  ;;set the random number that will be continuously reset

  	  ;;for the needed length
  	  
  	  ;;loop through both lists
		(loop for i from 0 to (- (list-length ind1) 1)
			;;keep track of the current element in ind1 
			do (setq curX (nth 0 (nth i ind1)))
			;;do (print curX)
			;;keep track of the current element in ind1 
			do (setq curY (nth 0 (nth i ind2)))
			;;do (print curY)
			;;create a random number between 0 and 1.0 inclusively
			do (setq rannum (random 1.1))
			;;do (print rannum)
			;;if the crossover probabilty is higher
			if (>= *crossover-probability* rannum)
			;;switch the elements in the two lists
			do (setf (nth 0 (nth i ind1)) curY)
			;;same if statement just so that you can have
			;;multiple do statements that comply with the if statement
			if (>= *crossover-probability* rannum)
			do (setf (nth 0 (nth i ind2)) curX)
			))


(defun gaussian-convolution (ind)
  ;;"Performs gaussian convolution mutation on the individual, modifying it in place.
  ;;Returns NIL."

  ;;; IMPLEMENT ME
  ;;;
  ;;; For mutation, see gaussian convolution (Algorithm 11) in
  ;;;                Essentials of Metaheuristics
  ;;; Keep in mind the legal minimum and maximum values for numbers.
  ;;; HINTS:
  ;;; Maybe a function or three in the utility functions above might be handy
  ;;; See also SETF

  
  ;;*mutation-probability*
  ;;*mutation-variance*
  
  ;;loop through both lists
		(loop for i from 0 to (- (list-length ind) 1)
			
			;;create a random number between 0 and 1.0 inclusively
			do (setq rannum (random 1.1))
			;;do (print rannum)
			
			;;if the mutation probabilty is higher
			if (>= *mutation-probability* rannum)
			;;get a new random number from the gaussian-random function provided
			do (setq n (gaussian-random 0 *mutation-variance*))
			
			;;just to see how the code is working
			;;if (>= *mutation-probability* rannum)
			;;do (print (nth i ind))
			
			;;while float-min > (nth i ind1) + n OR > float-max
			;;aka the n does not satisfy the condition
			if (>= *mutation-probability* rannum) do
			(loop while (or (< (+ n (nth 0 (nth i ind))) *float-min*) (> (+ n (nth 0 (nth i ind))) *float-max*))
				;; get a new n -> ;;[n = (setq rangaus (gaussian-random 0 *mutation-variance*))]
				;;and try again
				do (setq n (gaussian-random 0 *mutation-variance*))
				;;do (print n)
			)
			;;when the loop is broken aka n < max and n > min and
			;;that particular element is supposed to be mutated
			if (>= *mutation-probability* rannum) 
			;;mutate the individual based on the new value
			do (setf (nth 0 (nth i ind)) (+ n (nth 0 (nth i ind))))))

(defun float-vector-modifier (ind1 ind2)
  ;;"Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
  ;;then mutates the children.  *crossover-probability* is the probability that any
  ;;given allele will crossover.  *mutation-probability* is the probability that any
  ;;given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

    ;;; IMPLEMENT ME
    ;;; It's pretty straightforward.
    ;;; This function should first COPY the two individuals, then
    ;;; CROSS THEM OVER, then mutate the result using gaussian covolution,
    ;;; then return BOTH children together as a list (child1 child2)
    ;;;
    ;;; HINTS:
    ;;; For copying lists:  See the Lisp Cheat Sheet 
    ;;;                (http://cs.gmu.edu/~sean/lisp/LispCheatSheet.txt)
    
    ;;simple alorithm 
    
    ;;copy ind1
    (setq ind1COPY (copy-tree ind1))
    
    ;;copy ind2
    (setq ind2COPY (copy-tree ind2))
    
    ;;uniform cross them 
    ;;uniform-crossover (ind1 ind2) -> the copies not the originals
    (uniform-crossover ind1COPY ind2COPY)

    ;;mutate ind1
    ;;gaussian-convolution (ind)
    (gaussian-convolution ind1COPY)
    
    ;;mutate ind2
    ;;gaussian-convolution (ind)
    (gaussian-convolution ind2COPY)
    
    ;;return (child1 child2)
    (block nil (return (list ind1COPY ind2COPY))))

;; you probably don't need to implement anything here
(defun float-vector-sum-setup ()
  ;;"Does nothing.  Perhaps you might use this function to set
  ;;(ahem) various global variables which define the problem being evaluated
  ;;and the floating-point ranges involved, etc.  I dunno."

  ;;just added in the numbers that i'll need to use
	(defvar *crossover-probability* 0.1)
	(defvar *mutation-probability* 0.1) 
	(defvar *mutation-variance* 0.02)
	(defvar *float-vector-length* 20)
	(defvar *float-min* -5.12) 
	(defvar *float-max* 5.12))

;;REPORT (in comments) 
;;Basically the best thing about this project is it breaks down several 
;;algorithms down and allows you to use them, then combine them together. 
;;this is seen mainly in the vector algorithms. If you were to try and to
;;implement float- vector- modifier all on its own, it would leave a lot of room
;;room for error and bugs. Not to mention it would've been a nightmare just to
;;write. Tournament selector and float vector modifier were the easiest 
;;functions to complete (other than float-vector-sum-setup of course) becuase 
;;there really wasn't any logic or algorithms that had to go in to them, they 
;;just called in the functions that were called previously. I wrote uniform
;;crossover based on the algorithm that was in the text book. Assuming that
;;ind1 and ind2 are the same length, i go through that length and just keep 
;;generating random probabilities and comparing them to the given crossover 
;;probability. When you set crossover probability really low, like the one that
;;was given to us, most likely there won't be any crossovers. Espcially if the
;;vectors you choose for inputs are short. The higher the crossover probabilty
;;is, the more likely a crossover will happen. The longer the vectors are, the
;;more cross overs will will occur. Also because it uses random numbers every
;;result will always be different. Gaussian convolution was also based on an 
;;algorithm in the book. It has similar ideas of the cross over alogrithm only 
;;this time instead of crossing anything over, it is only given one individual,
;;and based on the mutation probabilty and the gaussian random it will either
;;mutate it or not. Float vector creator was pretty straight forward, I just 
;;made a helper function that creates a 1x1 list with a random float within
;;range, and it'll randomly decide if it should be positive or negative. then 
;;the main function just added all those 1x1 lists to the big vector and then
;;it returns the big vector. To complete tournament select, I just followed the
;;algorithms that were given in the book. I ASSUMED that population was a list 
;;of individuals and that fitnesses were floats. It also followed similar rules
;;aka the higher the mutation probabilty the more mutations would be done. as 
;;well as the more elements in your individual vector, the more mutations were 
;;going to happen. I was kind of confused on how evolve was supposed to be 
;;written. 