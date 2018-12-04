(defun  foo(l)
	(cond
		((null l) nil)
		((atom l) (list l))
		(t (append (foo (car l))
				 (foo (cdr l))))))