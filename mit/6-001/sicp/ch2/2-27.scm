###2.27###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))
-----------------------------------------------------------------------------------------------------------------

(define (reverse l)
  (define (reverse-iter remaining done)
    (if (null? (cdr remaining))
	(cons (car remaining) done)
	(reverse-iter (cdr remaining) (cons (car remaining) done))))
  (reverse-iter l (list)))

(define (deep-reverse l)
  (define (deep-reverse-iter remaining done)
    (cond ((null? remaining) done)
	  ((not (pair? remaining)) remaining)
	  (else (deep-reverse-iter (cdr remaining) (cons (deep-reverse (car remaining)) done)))))
  (deep-reverse-iter l (list)))

(define x (list (list 1 2 (list 3 4)) (list 5 6)))

(deep-reverse x)
;Value 29: ((6 5) ((4 3) 2 1))
