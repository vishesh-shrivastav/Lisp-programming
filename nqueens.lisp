; Steps to run:
; >(load "nqueens.lisp")
; >(nqueens 8)

; Function to determine if one piece is being threatened by another
(defun is_threatened (i j a b)
  (or (= i a)
      (= j b)
      (= (- i j) (- a b))
      (= (+ i j) (+ a b))))

; Function to determine if placement of a queen is valid
(defun is_conflicted (n m board)
  (cond  ((endp board) nil)
         ((is_threatened n
                  m
                  (first (first board))
                  (second (first board)))
          t)
         (t (is_conflicted n m (rest board)))))

; Function to print the board
(defun print_board (board)
  (format t "~%*")
  (print_horizontal_border board)
  (format t "*")
  (dotimes (row (length board))
    (format t "~%|")
    (dotimes (column (length board))
      (if (= column (second (assoc row board)))
          
        (format t " Q")
        (format t " .")))
    ; Print vertical border
    (format t " |"))
  (format t "~%*")
  (print_horizontal_border board)
  (format t "*"))

; Function to print horizontal border
(defun print_horizontal_border (board)
  (dotimes (n (+ 1 (* 2 (length board))))
    (format t "#")))

; Main function that solves the eight queen problem
; using hill-climb approach  
(defun nqueens (size &optional (board nil) (n 0) (m 0))
  (unless (= m size)
    ; Check if the placement of queens in board is valid
    (unless (is_conflicted n m board)
      (if (= (+ 1 n) size)
        ; Check if all queens are correctly placed
        ; If yes, print the solution
        (print_board (reverse (cons (list n m) board)))
        ; If not, proceed to the next row and recursively call nquees
        (nqueens size (cons (list n m) board) (+ 1 n) 0)))
    ; Move on to the next column and call nqueens recursively
    (nqueens size board n (+ 1 m))))