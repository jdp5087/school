PROMPT:
------------------------------------------------------------
Exercise 2.39.   Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left from exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) <??>) nil sequence))
------------------------------------------------------------


  (fold-right (lambda (x y) (cons 

(define (reverse sequence)
  (fold-right (lambda (x y)
		(if (null? y)
		    (cons x ())
		    (cons (car y) (cons x (cdr y)))))
	      ()
	      sequence))

(reverse (list 1 2 3))

(define (shorten sequence)
  (accumulate (lambda (x y)
		(if (null? y)
		    (cons () ())
		    (cons x y)))
	      ()
	      sequence))

(shorten (list 1 2 3))

