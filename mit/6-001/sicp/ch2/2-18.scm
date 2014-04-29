###2.18###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order.
-----------------------------------------------------------------------------------------------------------------

(define (reverse l)
  (define (reverse-iter remaining done)
    (if (null? (cdr remaining))
	(cons (car remaining) done)
	(reverse-iter (cdr remaining) (cons (car remaining) done))))
  (reverse-iter l (list)))

(reverse (list 1 2 3 4))
