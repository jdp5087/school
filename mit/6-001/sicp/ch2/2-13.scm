PROMPT:
------------------------------------------------------------
Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.
------------------------------------------------------------

The resistance of the new interval can be described by:

Wp = (1/((1/W1)+(1/W2)))

and the new center point can be calculated by:

Cp = ((C1*C2)/2)*Wp

since center = width/resistance, we have:

Wp/Rp = ((C1*C2)/2)*Wp

Rp = 1/((C1*c2)/2)

and therefore:

Rp = 2/(C1*C2)

We can also describe this by

Rp = 2/((W1*W2)/(R1/R2)) 

Based on the information we start with, we can now find the resistance of two circuits in parallel in two ways.
I found this answer in a rather circuitous manner, so the mathematical argument might not be exactly linear, but it works.

Here's an implementation using the centers of two intervals

(define (par-res i j)
  (* 100 (/ 2 (* (center i) (center j)))))

(let ((i (make-center-percent 6.8 10))
      (j (make-center-percent 4.7 5)))
  (par-res i j))
;Value: 6.25782227784731

