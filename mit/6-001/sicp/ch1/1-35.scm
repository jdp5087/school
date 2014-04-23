###1.35###
------------------------------------------------------------
Exercise 1.35.  Show that the golden ratio  (section 1.2.2) is a fixed point of the transformation x   1 + 1/x, and use this fact to compute  by means of the fixed-point procedure.
------------------------------------------------------------

Good old wikipedia tells us that the golden ratio is defined as:

(a+b)/a = a/b = x

which we can translate to

1+ b/a = 1 + 1/x (because b/a is the reciprocal of x)

so we want to know at what point x = 1 + 1/x, which will be precisely when x is equal to the golden ratio. To see this, consider:

lemma. The fixed point of 1 + 1/x is equal to the golden ratio.

Proof. By implication. Assume that the golden ratio can be defined as 1 + 1/x. Now, we can substitute value of phi and the reciprocal of phi.

now we know that phi = 1+sqrt(5)/2 and the 1/phi = 1-sqrt(5)/2, therefore:

x = 1 + 1/x

phi = 1 + 1/phi

1+sqrt(5)/2 = 1 + 1-sqrt(5)/2

1+sqrt(5)/2 - 1-sqrt(5)/2 = 1

1+1/2 = 1

1 = 1

Therefore, by implication, if the fixed point of the function f(x) = 1 + 1/x is precisely the golden ratio.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       0.1))

(golden-ratio)
;Value: 1.6180365296803654
