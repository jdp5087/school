###2.26###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.26.  Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)

(cons x y)

(list x y)
-----------------------------------------------------------------------------------------------------------------

(define x (list 1 2 3))
(define y (list 4 5 6))

I'll try to reason out the result first, and then check my answers.

(append x y) should return a list (1 2 3 4 5 6), which will be accomplished by the procedure:

(define (append l1 l2)
  (if (null? (cdr l1))
      (cons (car l1) l2)
      (cons (car l1)
	    (append (cdr l1) l2))))

(append x y)
;Value 20: (1 2 3 4 5 6)

(cons x y) should return ((1 2 3) 4 5 6), because (1 2 3) will just become the first item of the list.

(cons x y)
;Value 21: ((1 2 3) 4 5 6)

(list x y) should return ((1 2 3) (4 5 6)), which is like calling:

(cons x (cons y (list)))

(list x y)
;Value 22: ((1 2 3) (4 5 6))

