PROMPT:
------------------------------------------------------------
Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.
------------------------------------------------------------

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

(let ((i1 (make-interval 6.12 7.48))
      (i2 (make-interval 4.47 4.94)))
  (sub-interval i1 i2))


This procedure simply takes two intervals as arguments and returns a new interval, which has a lower bound of the difference between lower bounds of x and y, and its upper bound is defined as the difference between the upper bounds of x and y.

