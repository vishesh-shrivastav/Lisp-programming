; Name: Vishesh Shrivastav
; steps to run:
; 1) Navigate to submission folder with all lisp file
; 2) start clisp
; 3) load the lisp file as (load "eightpuzzle.lisp")
; 4) call main function as (solve_puzzle '#(1 2 3 4 5 6 7 8 E))

; Create a structure to store the attributes of the state
(defstruct state_node
	state    ;array to store the puzzle elements
	state_id ;to store state id
	h ; Value of heuristic function
	g ; Cost of path from starting node to current state
	f ; Total cost
)

; Set goal_state as constant
(setf goal_state '#(1 2 3 4 5 6 7 8 0))

; Array to store all the states that are generated in the solution
(setf *all_state* (make-array 0 :fill-pointer t :adjustable t))

; Array to store all the states that have been visited
(setf visited_states (make-array 0 :fill-pointer t :adjustable t))

; Feasible flag to check the feasibility of the given problem
(setf feasible_flag 0) 

; Initial state of the puzzle as a 1D array or list
(setf initial_state (make-array 9))

; Main function
(defun solve_puzzle(start_state) 

	; Define initial_state list from start_state
	; Used to check feasibility
	(loop for i from 0 to 8 do 
		(if (eq 'E (aref start_state i))
			(setf (aref initial_state i) 0)		
		(setf (aref initial_state i) (aref start_state i))
		)		
	)

	; Convert input list to array
	(setf initial_state_arr (listToArray(start_state)))
	; Check if the puzzle is feasible
	(setf feasible_flag (is_feasible initial_state)) 

	(if (/= 0 feasible_flag)
		(progn 
			(setf current_state initial_state_arr)
			(vector-push-extend (make-state_node
				:state current_state
				:state_id (length *all_state*)
				; compute heuristic value and store it in h
				:h (calculate_manhattan_distance current_state)
				:g 0
				:f (calculate_manhattan_distance current_state)) *all_state*)
			(solve_it)
			(setf expanded_nodes (-(length visited_states) 1))
			(format t "~%Nodes expanded: ~S" expanded_nodes)
		)
		(progn
			(format t "~%Infeasible puzzle")
		)
	)
)

; check if the puzzle is feasibility by checking the if inversion pair count is odd or not
(defun is_feasible(check_state)
	(setf inv_count (inversion_count check_state))
			(if (oddp inv_count)
				0
				1
			)	
)

; Function to find the position of empty spot
(defun find_empty(check_state) 
	(setf N (sqrt(length check_state)))
	(loop for i from (1- (length check_state)) downto 0 do
		(if (= 0 (aref check_state i))
			(mod i N)
		)
	)
)

; Helper function to calculate inversion count
(defun inversion_count(new_state)
	(setf inv_count 0)
	(loop for i from 0 to (1- (1- (length new_state))) do
		(loop for j from (+ i 1) to (1- (length new_state)) do
			(if (and (/= 0 (aref new_state j)) (/= 0 (aref new_state i)) (> (aref new_state i)(aref new_state j) ))
				(incf inv_count) ; increment count of inverse pairs
			)
		)
	)
	inv_count
)

; Function to sort all possible state
; using quick sort
(defun sort_all_states (low high)
	(if (< low high)
		(progn
			(setf p (partition low high))
			(sort_all_states low (- p 1))
			(sort_all_states (+ p 1) high)
		)
	)	
)

; Helper function for paritioning in quick sort
(defun partition (low high)
	(setf pivot (state_node-f (aref *all_state* high)))
	(setf i (- low 1))
	(loop for j from low to (- high 1) do
		(if (>= (state_node-f (aref *all_state* j)) pivot)
			(progn
				(incf i)
				(rotatef (aref *all_state* i) (aref *all_state* j))
			)
		)
	)
	(rotatef (aref *all_state* (+ 1 i)) (aref *all_state* high))
	(+ i 1)
)

; Function to solve the puzzle if is feasible
(defun solve_it()
	; First, sort the states
	(sort_all_states 0 (1- (length *all_state*)))
	; Then, define current state
	(setq current_state (vector-pop *all_state*))


	(let ((temp_state (copy-seq (state_node-state current_state))))
		(display_current_state temp_state)
		(vector-push-extend temp_state visited_states)
		(if (equalp goal_state temp_state)
			(progn
				(format t "~% Goal state reached")
			)

			; Generate all possible states from current state by 
			; allowing moves in all possible directions
			(progn
				(loop for i from 0 to 8 do
					(if (= 0 (aref (state_node-state current_state) i))
						(progn
								(if (and (/= i 2) (/= i 5) (/= i 8))  
									(move current_state i (+ i 1))
								)
								(if (and (/= i 6) (/= i 7) (/= i 8))
									(move current_state i (+ i 3))
								)
								(if (and (/= i 0) (/= i 3) (/= i 6))
									(move current_state i (- i 1))
								)
								(if (and (/= i 0) (/= i 1) (/= i 2))
									(move current_state i (- i 3))
								)
						)
					)
				)
				; Call solve_it recursively until we reach the goal state
				(solve_it)
			)
		)
	)
)

; Display contents of current puzzle state
(defun display_current_state(current_state)
	(setf sqrt_size(sqrt (length current_state)))
	(format t "~%~%")
	(loop for i from 0 to (- (length current_state) 1) do
		(if (or (= 3 i) (= 6 i))
			(format t "~%")
		)
		(if (or (= 0 i) (= 3 i) (= 6 i))
			(format t "|")
		)
	(if (= 0 (aref current_state i))
		(format t "E|")
 		(format t "~a|" (aref current_state i))
	)
	)
)

; Find the next move of the puzzle from the current state
(defun move(move_state empty_spot block)
	(setf temp_state (copy-seq (state_node-state move_state)))
	(rotatef (aref temp_state empty_spot) (aref temp_state block))
	(if (= 1 (is_visited temp_state))
		(vector-push-extend(make-state_node
					:state temp_state
					:state_id (length *all_state*)
					:g (+ 0.1 (state_node-g move_state))
					:h (calculate_manhattan_distance temp_state)
					:f (+ (+ 0.1 (state_node-g move_state)) (calculate_manhattan_distance temp_state)))
					*all_state*
		)	
	)
)

; Check if the current state reached has already been visited
(defun is_visited(temp_state)
	(loop for i from 0 to (1- (length visited_states)) do
		(if (equalp temp_state (aref visited_states i))
			(return-from is_visited 0)	
		)
	)
	1
)

; Function to convert list into an array
(defun listToArray (foo)
	(setf bar (make-array '(3 3)))
	(dotimes (i 3)
		(dotimes (j 3)
			(setf (aref bar i j) (elt foo 0))
			(setf foo (remove (elt foo 0) foo))
		)
	)
	(return-from listToArray bar)
)

; Convert goal state list to array
;(setf goal_as_arr (listToArray goal_state))

; Function to get row of given element in goal state
(defun getrow (e current)
	(dotimes (i 3)
		(dotimes (j 3)
			(cond
				((equal e (aref current i j)) (setf row i))
			)
		)
	)
	(write row)
	(return-from getrow row)
)

; Function to get column of given element in goal state
(defun getcolumn (e current)
	(dotimes (i 3)
		(dotimes (j 3)
			(cond
				((equal e (aref current i j)) (setf column j))
			)
		)
	)
	(write column)
	(return-from getcolumn column)
)

; (setq goal_state_as_arr (listToArray goal_as_arr))

; Heuristic function
(defun calculate_manhattan_distance (current_state_of_puzzle)
	;; Function to calculate manhattan distance between two arrays
	(setf manhattan_distance 0)
	(dotimes (i 3)
		(dotimes (j 3)
			(setf val (aref current_state_of_puzzle i j))

			; get row of current item in goal state
			(setf row (getrow val goal_state_as_arr)) 

			; get column of current item in goal state
			(setf column (getcolumn val goal_state_as_arr))

			(setf rowdiff (abs (- row i)))
			(setf columndiff (abs (- column j)))
			(setf sum_of_row_col (+ rowdiff columndiff))
			(setf manhattan_distance ( + manhattan_distance (+ sum_of_row_col)))
		)
	)

	(return-from calculate_manhattan_distance manhattan_distance)
)