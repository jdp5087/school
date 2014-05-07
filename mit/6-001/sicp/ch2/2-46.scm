###2.46###
PROMPT:
------------------------------------------------------------
Exercise 2.46.  A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar
------------------------------------------------------------

(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
z  (car v))
(define (ycor-vect v)
  (cdr v))
(define (add-vect v w)
  (cons (+ (xcor-vect v) (xcor-vect w))
	(+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (cons (- (xcor-vect v) (xcor-vect w))
	(- (ycor-vect v) (ycor-vect w))))
(define (scale-vect v s)
  (cons (* (xcor-vect v) s)
	(* (ycor-vect v) s)))

(let ((v1 (make-vect 1 1))
      (v2 (make-vect 2 2)))
  (display (add-vect v1 v2))
  (display (sub-vect v2 v1))
  (display (scale-vect v1 5)))

;(3 . 3)(1 . 1)(5 . 5)
;Unspecified return value

	  