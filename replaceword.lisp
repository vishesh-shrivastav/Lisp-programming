(defun replaceword (symb lst)
	; Base condition - If list is empty, return it
	(cond ((null lst) lst)
		  ; If first element of the list is the symbol, set 'YYYY' as the first item in returning
		  ; list and recursively call the function over rest of the list
		  ((eq symb (car lst)) (cons `YYYY (replaceword symb (cdr lst))))
		  ; Else, set first element of returning list as car of given list and recursively
		  ; call the function over the rest of the list
		  (t (cons (car lst) (replaceword symb (cdr lst))))))