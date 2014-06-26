###3.17###
PROMPT:
------------------------------------------------------------
Exercise 3.17.  Devise a correct version of the count-pairs procedure of exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)
------------------------------------------------------------
(define (in struct structs)
  (cond ((null? structs) false)
	((eq? struct (car structs)) true)
	(else (in struct (cdr structs)))))

(define (count-pairs struct)
  (let ((known-structs '()))
    (define (iter s)
      (cond ((null? s) 0)
	    ((in s known-structs) 0)
	    ((not (pair? s)) 0)
	    (else
	     (begin
	       (set! known-structs (cons s known-structs))
	       (+ 1 (iter (car s)) (iter (cdr s)))))))
    (iter struct)))


(define w (list 'a 'b 'c))
(count-pairs w)
;Value: 3

(define first-part (cons 'a '()))
(define y (cons (cons 'a first-part) first-part))
(count-pairs y)
;Value: 3

(define a (cons 'a '()))
(define b (cons a a))
(define c (cons b b))
(count-pairs c)
;Value: 3



