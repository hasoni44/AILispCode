(defun make-initial-state (initial-puzzle-situation)
    "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
    (cons (concatenate 'simple-vector initial-puzzle-situation 
            (list (position 9 initial-puzzle-situation))) nil))

(defun create-random-state (num-moves)
    "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
    (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
        (dotimes (x num-moves)
            (let ((moves (elt *valid-moves* (empty-slot puzzle))))
                (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
        (build-state puzzle nil)))

(defmacro depth (state)
    "Returns the number of moves from the initial state 
    required to get to this STATE"
    `(1- (length ,state)))

(defmacro puzzle-from-state (state)
    "Returns the puzzle (an array of 10 integers) from STATE"
    `(car ,state))

(defmacro previous-state (state)
    "Returns the previous state that got us to this STATE"
    `(cdr ,state))

(defmacro empty-slot (puzzle)
    "Returns the position of the empty slot in PUZZLE"
    `(elt ,puzzle 9))

(defun swap (pos1 pos2 puzzle)
    "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 9 is updated appropriately."
    (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
        (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
        (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
        (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
            (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
        puz))

(defparameter *valid-moves* 
    #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
    "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defmacro foreach-valid-move ((move puzzle) &rest body)
    "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
    `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
        ,@body))

(defun make-move (move puzzle)
    "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
    (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (when (find move moves) (swap move (empty-slot puzzle) puzzle))))

(defmacro build-state (puzzle previous-state)
    "Builds a state from a new puzzle situation and a previous state"
    `(cons ,puzzle ,previous-state))

(defmacro foreach-position ((pos puzzle) &rest body)
    "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
    (let ((x (gensym)))
        `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
            ,@body))))

(defun print-puzzle (puzzle)
    "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
    (let (lis)
        (foreach-position (pos puzzle)
            (if (= pos 9) (push #\space lis) (push pos lis)))
        (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
    puzzle)

(defun print-solution (goal-state)
    "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
    ;; first let's define a recursive printer function
    (labels ((print-solution-h (state)
                (print-puzzle (puzzle-from-state state)) (terpri)
                (when (previous-state state) (print-solution-h (previous-state state)))))
        ;; now let's reverse our state list and call it on that
        (if (equalp goal-state 'failed) 
            (format t "~%Failed to find a solution")
            (progn
                (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
                (print-solution-h (reverse goal-state))))))

;;------------------------------------------------------------------------------
;;NOTES:
;;The history list ought to contain PUZZLES, not states.
;;the queue ought to contain STATES

;;(load "~/Desktop/480-HW4/utility.lisp")
;;(load "~/Desktop/480-HW4/queue.lisp")

;;make the hashmap a global variable so it can be used in the search 
;;and the encueing functions
(defparameter history (make-hash-table :test 'equalp))

(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
    "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited states.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."

;; hints: The history list ought to contain PUZZLES, not states.
;; However, the queue ought to contain STATES, which in this case consist of
;; conses consisting of the PUZZLE as the car, and the puzzle's parent's cons as the cdr.
;; You should use #'equalp to test for equality between puzzles in the history list.
;; You should also add the puzzle to the history list at exactly the same time
;; you add its corresponding state to the queue.


;;; GeneralSearch(InitialState, GoalTest, EnqueueingFunction, MaxIterations)
;;;   make new empty queue
	(setf queue (make-empty-queue))
;;;   make new empty history
	(defparameter history (make-hash-table :test 'equalp))
;;;   iterations <- 0
	(setf iterations 0)
;;;   state <- InitialState
;;;
;;;   EnqueuingFunction(queue,state)
	 (setf EIQ (funcall enqueueing-function initial-state queue))
;;;   add state to history

	 ;;if max iterations not given, set it to 100,000 -> it'll never get this far
	 ;;if it gets this far, there is no solution
	  (if (eq maximum-iterations nil)
	  	  (setf maximum-iterations 1000000))

	  (if (> (nth 1 EIQ) maximum-iterations)
	  	  (print "FAILURE")
	  	  ;;print the num of iterations
	  	  (prog 
	  	  	  (print (nth 1 EIQ))
	  	  	  ;;return goal state
	  	  	  (return-from general-search (nth 0 EIQ)))))

(defun goal-p (state)
    ;;"Returns T if state is a goal state, else NIL.  Our goal test."

    ;;the goal is '(1 2 3 4 5 6 7 8 9)
    ;;if car of a state is the goal, return T -> anything else returns NIL
    
    ;;NOTE: i did the code in this (slighty longer and less efficient way
    ;;because when i tried to do a normal equality statement 
    ;;it did not work
    ;;The reason behind that is the make initial state function for some 
    ;;reason adds a random number to the end of the array 
    ;;so i just keep checking the first 9 digits and if they're all equal
    ;;T will automatically be returned, if anyhting is not equal it will
    ;;return NIL
    (and (and (and (and (and (and (and (and 
								(eq 1 (aref (puzzle-from-state state) 0)) 
								(eq 2 (aref (puzzle-from-state state) 1))) 
							(eq 3 (aref (puzzle-from-state state) 2))) 
						(eq 4 (aref (puzzle-from-state state) 3))) 
					(eq 5 (aref (puzzle-from-state state) 4))) 
				(eq 6 (aref (puzzle-from-state state) 5))) 
			(eq 7 (aref (puzzle-from-state state) 6))) 
		(eq 8 (aref (puzzle-from-state state) 7))) 
	(eq 9 (aref (puzzle-from-state state) 8))))
;;------------------------------------------------------------------------------
;;-------------------------HELPER LIST/PUZZLE FUNCTIONS-------------------------
;;HELPER FUNCTION
(defun list-to-array (puzzle)
	;;create a new list
	(let ((returnedlist '()))
		;;go throught the puzzle
		(loop for x from 0 to 8
			;;just add every function from the puzzle in to the returned list
			do (setq returnedlist (append returnedlist (list (aref puzzle x))))
			;;return the list
		finally (return returnedlist))))

;;HELPER FUNCTION
(defun list-to-2dlist (l1)
	(let ((newlist '()))
	(setq newlist (append newlist (list (list (nth 0 l1) (nth 1 l1) (nth 2 l1)))))
	(setq newlist (append newlist (list (list (nth 3 l1) (nth 4 l1) (nth 5 l1)))))
	(setq newlist (append newlist (list (list (nth 6 l1) (nth 7 l1) (nth 8 l1)))))))

;;HELPER FUNCTION
(defun minny (x)
	;;find minimum value of list x
	(apply 'min x))

;;HELPER FUNCTION
(defun valid-moves (curPuzzle)
	;;create new list -> will be list of lists
	;;get current puzzle from state
	;;deep copy puzzle and send it through list-to-2dlist
	;;use -> swap (pos1 pos2 puzzle)
	;;NOTE: i tried to use empty slot function but it was having bugs for some reason
	(let ((stateslist '()))
		
	(cond
  	  ;;if 9 is in index 0
  	  ;;swap 0,1 
	  ;;swap 0,3
  	  ((eq (aref curPuzzle 0) 9) (setq stateslist (append stateslist (list (swap 0 3 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 0 1 curPuzzle)))))
  	  
  	  ;;if 9 is in index 1
  	  ;;swap 0,1
	  ;;swap 1,2
	  ;;swap 1,4
  	  ((eq (aref curPuzzle 1) 9) (setq stateslist (append stateslist (list (swap 1 4 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 1 2 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 1 0 curPuzzle)))))
  	  
  	  ;;if 9 is in index 2
  	  ;;swap 2,1
	  ;;swap 2,5
  	  ((eq (aref curPuzzle 2) 9) (setq stateslist (append stateslist (list (swap 2 5 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 2 1 curPuzzle)))))
  	  
  	  ;;if 9 is in index 3
  	  ;;swap 3,6
	  ;;swap 3,4
	  ;;swap 3,0
  	  ((eq (aref curPuzzle 3) 9) (setq stateslist (append stateslist (list (swap 3 6 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 3 4 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 3 0 curPuzzle)))))
  	  
  	  ;;if 9 is in index 4
  	  ;;swap 4,1
	  ;;swap 4,3
	  ;;swap 4,7
	  ;;swap 4,5
  	  ((eq (aref curPuzzle 4) 9) (setq stateslist (append stateslist (list (swap 4 7 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 4 5 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 4 3 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 4 1 curPuzzle)))))
  	  
  	  ;;if 9 is in index 5
  	  ;;swap 5,2
	  ;;swap 5,8
	  ;;swap 5,4
  	  ((eq (aref curPuzzle 5) 9) (setq stateslist (append stateslist (list (swap 5 8 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 5 4 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 5 2 curPuzzle)))))
  	  
  	  ;;if 9 is in index 6
  	  ;;swap 6,3
	  ;;swap 6,7
  	  ((eq (aref curPuzzle 6) 9) (setq stateslist (append stateslist (list (swap 6 7 curPuzzle))))
  	  	  							 (setq stateslist (append stateslist (list (swap 6 3 curPuzzle)))))
  	  
  	  ;;if 9 is in index 7
  	  ;;swap 7,4
	  ;;swap 7,6
	  ;;swap 7,8
  	  ((eq (aref curPuzzle 7) 9) (setq stateslist (append stateslist (list (swap 7 8 curPuzzle))))
  	  	 							 (setq stateslist (append stateslist (list (swap 7 6 curPuzzle))))
  	  	 							 (setq stateslist (append stateslist (list (swap 7 4 curPuzzle)))))
  	  
  	  ;;if 9 is in the 8 index
  	  ;;swap 8,7
  	  ;;swap 8,5
  	  ((eq (aref curPuzzle 8) 9) (setq stateslist (append stateslist (list (swap 8 7 curPuzzle))))
  	  	 							 (setq stateslist (append stateslist (list (swap 8 5 curPuzzle))))))))

;;HELPER FUNCTION
(defun list-manhattan-distance (listy)
	(let ((distance 0)
		 (l1 '((1 2 3) (4 5 6) (7 8 9)))
		 ;;deep copy
		 (l2 (list-to-2dlist (copy-tree listy))))
		;;deep copy l1
		;;put it through list-to-2dlist function
			
		;;find the distance between the deepl1 and l2 
		;;based on manhattan hueristic
		;;return the distance
		(loop for x1 from 0 to 2
			append (loop for y1 from 0 to 2
				append (loop for x2 from 0 to 2
					append (loop for y2 from 0 to 2
						;; if the x and y are not the same but they point to the same value and that value is not 9
						if (and (and (eq (nth y1 (nth x1 l1)) (nth y2 (nth x2 l2))) (not (and (eq x1 x2) (eq y1 y2)))) (not (eq 9 (nth y1 (nth x1 l1)))))
						;; find the manhattan distance 
						do (setf distance (+ distance (+ (abs (- x2 x1)) (abs (- y2 y1))))))))
			;;return the distance when you finally finish
			finally (return distance))))
;;------------------------------------------------------------------------------
(defun dfs-helper (childlist history)
	
	;;go through child list
	(loop for x in childlist
	;;change (setf (aref current item 9) 0)
		do (setf (aref x 9) 0)
	;;if child not in history return it
		(if (not (gethash x history))
			;;return the child
 				(return-from dfs-helper x)))
	;;if nothing returned after loop executes, just return first item
	(return-from dfs-helper (nth (random (list-length childlist)) childlist)))

(defun dfs-enqueuer (state queue)
    "Enqueues in depth-first order"
    ;;LIFO -> stack
    ;;(enqueue-at-front state)
    
    (setq child nil)
    (setf (aref (car state) 9) 0)
    (setf (gethash (car state) history) 0)
    (setf iterators 0)
    
    ;;while goal has not been reached
    (loop while (not (goal-p state))
		;;keep getting a child -> not in history
		do (setq child (dfs-helper (valid-moves (car state)) history))
		;;add car to history
		do (setf (aref child 9) 0)
		do (setf (gethash child history) 0)
		;;move car -> cdr
		do (setf (cdr state) (car state))
		;;move new child -> car
		do (setf (car state) child)
		;;add state to queue
		(setf statecopy (copy-tree state))
		(enqueue-at-front queue statecopy)
		do (incf iterators))
	(return-from dfs-enqueuer (list state iterators)))
;;------------------------------------------------------------------------------
(defun bfs-enqueuer (state queue)
    "Enqueues in breadth-first order"
    
    ;;FIFO queue -> (enqueue-at-end state)
    
    (setf childlist '())
    ;;the current parent list -> during first iteration only one parent
    (setf (aref (car state) 9) 0)
    (setf parentlist '())
    (setf parentlist (append parentlist (list (car state))))
    
    (setf (gethash (car state) history) 0)
    (setf iterators 0)
    
    ;;while the goal hasn't been reached
    (loop while (not (goal-p state))
    	;;go through the parent nodes
    	do (loop for p in parentlist
    		;;find all of their childrent
    		do (setf childlist (append childlist (valid-moves p))))
    	do (loop for c in childlist
    		;;set the last element to 0
    		do (setf (aref c 9) 0)
    		;;if the child isn't in the history
    		(if (not (gethash c history)) 
    			(progn
				;;move car -> cdr
				(setf (cdr state) (car state))
				;;move current-child -> car
				(setf (car state) c)
				;;add current child to history
				(setf (gethash c history) 0)
				;;add state to queue
				(setf statecopy (copy-tree state))
				(enqueue-at-end queue statecopy)
				;;if we are currently at the goal state
				(if (goal-p state)
					;;return-from/exit the function
					(return-from bfs-enqueuer nil)))
			;;if c is in history, remove it from the child list
			(remove c childlist)))
    	;;move childlist to parent list
    	do (setf parentlist childlist)
    	;;reset the child list
    	do (setf childlist '())
    	do (incf iterators))
    (return-from bfs-enqueuer (list state iterators)))
;;------------------------------------------------------------------------------
;;helper function
(defun distances (childlist)
	;;find the manhattan distances of a childlist 
	(let ((distance-list '()))
		;;go through child list
		(loop for i in childlist do
			;;for each child, find its distance and add it to a list which will eventually be returned
			(setf distance-list (append distance-list (list (list-manhattan-distance (list-to-array i)))))
			finally (return distance-list))))

;;helper function
(defun remover (n listy)
	;;remove element in list based on its index
    (append (subseq listy 0 n) (subseq listy (+ n 1) (length listy))))

;;helper function
(defun new-manhattan (childlist history)
	;;set needed variables
	(setq distance-list (distances childlist))
	(setq backup '())
	
	;; go through childlist
	(loop while (not (equalp childlist nil))
		;;find the current min -> set to minChild
		do (setf minDist (minny distance-list))
		do (setf thepos (position minDist distance-list))
		do (setf (aref (nth thepos childlist) 9) 0)
		;;if not in history 
		(if (not (gethash (nth thepos childlist) history))
			;;return it
			(return-from new-manhattan (nth thepos childlist)) 
			;;else
			(progn
			;;add the child to the backup list
			(setf backup (append backup (list (nth thepos childlist))))	
			;;remove the child from the child list
			(setf childlist (remover thepos childlist))
			;;remove the istance from the distnace list
			(setf distance-list (remover thepos distance-list)))))
	;;if nothing is ever returned, return the first element from backup list
	(return-from new-manhattan (nth (random (list-length backup)) backup)))

;;helper function
(defun manhattandistance (state)
	;;manhattan distance function from state
	(list-manhattan-distance (list-to-array (car state))))

(defun manhattan-enqueuer (state queue)
    "Enqueues by manhattan distance"
    ;;enqueue-by-priority
    ;;create a childlist
    (setf childlist '())
    (setf (aref (car state) 9) 0)
    (setf (gethash (car state) history) 0)
    (setf iterators 0)
    
    (loop while (not (goal-p state))
    	;;find the possible children of the current 
    	do (setf childlist (valid-moves (car state)))
    	;;find the most suitable move based on helpr funciton
    	do (setf lowest (new-manhattan childlist history))
    	;;move the current car to cdr
    	do (setf (cdr state) (car state))
    	;;move the new lowest move to car
    	do (setf (car state) lowest)
    	;;add the new puzzle to history
    	do (setf (gethash lowest history) 0)
    	;;add the new state to the queue
    	(setf statecopy (copy-tree state))
    	do (enqueue-by-priority queue #'manhattandistance statecopy)
    	do (incf iterators))
    (return-from manhattan-enqueuer (list state iterators)))
;;------------------------------------------------------------------------------

;;HELPER FUNCTION
(defun num-differences (l1)
	(let ((cur 0))
		;;go through both lists, while ignoring 9
		(loop for i from 1 to 8
			for j in l1 do
			;;anytime there is an inequality, increment the counter
			(if (eq i j) (+ cur 0) (incf cur))
			;;return the counter in the end
			finally (return cur))))

;;helper function
(defun numdistances (childlist)
	;;find the manhattan distances of a childlist 
	(let ((distance-list '()))
		;;go through child list
		(loop for i in childlist do
			;;for each child, find its distance and add it to a list which will eventually be returned
			(setf distance-list (append distance-list (list (num-differences (list-to-array i)))))
			finally (return distance-list))))

;;helper function
(defun new-num-out (childlist history)
	;;set needed variables
	(setq distance-list (numdistances childlist))
	(setq backup '())
	
	;; go through childlist
	(loop while (not (equalp childlist nil))
		;;find the current min -> set to minChild
		do (setf minDist (minny distance-list))
		do (setf thepos (position minDist distance-list))
		do (setf (aref (nth thepos childlist) 9) 0)
		;;if not in history 
		(if (not (gethash (nth thepos childlist) history))
			;;return it
			(return-from new-num-out (nth thepos childlist)) 
			;;else
			(progn
			;;add the child to the backup list
			(setf backup (append backup (list (nth thepos childlist))))
			;;remove the child from the child list
			(setf childlist (remover thepos childlist))
			;;remove the istance from the distnace list
			(setf distance-list (remover thepos distance-list)))))
	;;if nothing is ever returned, return the first element from backup list
	(return-from new-num-out (nth (random (list-length backup)) backup)))

;;helper function
(defun numoutstate (state)
	;;helper function to be put in the queue by priority function
	(num-differences (list-to-array (car state))))

(defun num-out-enqueuer (state queue)
    "Enqueues by number of tiles out of place"
    ;;(num-differences (list-to-array
    ;;enqueue-by-priority
    
    (setf (aref (car state) 9) 0)
    (setf (gethash (car state) history) 0)

    ;;create a childlist
    (setf childlist '())
    (setf iterators 0)
    
    (loop while (not (goal-p state))
    	;;find the possible children of the current 
    	do (setf childlist (valid-moves (car state)))
    	;;find the most suitable move based on helpr funciton
    	do (setf lowest (new-num-out childlist history))
    	;;move the current car to cdr
    	do (setf (cdr state) (car state))
    	;;move the new lowest move to car
    	do (setf (car state) lowest)
    	;;add the new puzzle to history
    	do (setf (gethash lowest history) 0)
    	;;add the new state to the queue
    	(setf statecopy (copy-tree state))
    	do (enqueue-by-priority queue #'numoutstate statecopy)
    	do (incf iterators))
    (return-from num-out-enqueuer (list state iterators)))