;; Custom function for concatenating two lists
(defun concat (li1 li2)
  ; If first list is empty, return second list
  (cond ((null li1) li2)
  		; Else, store first element of list one and recursively "concat" rest of list 1 and list 2 
    	(t (cons (car li1) (concat (cdr li1) li2))))) 

;; Function for flattening a list - uses 'concat' defined above
(defun functionatom (x)
  (cond ((null x) x) ;; Base condition - if list is empty, return empty list
        ((atom x) (list x)) ;; If list has only one element, return it
        ;; Else if flattening pairs, return a list by concatenating the flattening of its car with the flattening of its cdr
        (t (concat (functionatom (car x)) (functionatom (cdr x))))))
