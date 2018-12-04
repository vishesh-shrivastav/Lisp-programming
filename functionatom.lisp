(defun  foo(l)
	(cond
		((null l) nil)
		((atom l) (list l))
		(t (append (foo (car l)) ; trying with append
				 (foo (cdr l))))))
