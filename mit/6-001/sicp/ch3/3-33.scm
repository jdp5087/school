###3.33###
PROMPT:
------------------------------------------------------------
Exercise 3.33.  Using primitive multiplier, adder, and constant constraints, define a procedure averager that takes three connectors a, b, and c as inputs and establishes the constraint that the value of c is the average of the values of a and b. 
------------------------------------------------------------

(define (averager a b c)
  (let ((u (make-connector))
	(v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))
	
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "a-connector" a)
(probe "b-connector" b)
(probe "c-connector: Average" c)

(forget-value! a 'user)
(forget-value! b 'user)

(averager a b c)

(set-value! a 9 'user)
Probe: a-connector = 9
;Value: done

(set-value! b 11 'user)
Probe: c-connector: Average = 10
Probe: b-connector = 11
;Value: done


