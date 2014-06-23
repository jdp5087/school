###2.95###
-----------------------------------------------------------------------------------------------------------------
Exercise 2.95.  Define P1, P2, and P3 to be the polynomials

P1: x^2 - 2x + 1
P2: 11x^2 + 7
P3: 13x + 5

Now define Q1 to be the product of P1 and P2 and Q2 to be the product of P1 and P3, and use greatest-common-divisor (exercise 2.94) to compute the GCD of Q1 and Q2. Note that the answer is not the same as P1. This example introduces noninteger operations into the computation, causing difficulties with the GCD algorithm.61 To understand what is happening, try tracing gcd-terms while computing the GCD or try performing the division by hand.
-----------------------------------------------------------------------------------------------------------------

(define p1 (make-polynomial 'x '(sparse (2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '(sparse (2 11) (0 7))))
(define p3 (make-polynomial 'x '(sparse (1 13) (0 5))))

(define Q1 (mul p1 p2))
(define Q2 (mul p1 p3))

q1
;Value 68: (polynomial x sparse (4 11) (3 -22) (2 18) (1 -14) (0 7))
q2
;Value 69: (polynomial x sparse (3 13) (2 -21) (1 3) (0 5))



;; I set a trace inside the install-polynomial package on gcd-terms like so: (trace gcd-terms)
;; and here's the output:


(greatest-common-divisor q1 q2)
[Entering #[compound-procedure 66 gcd-terms]
    Args: (sparse (4 11) (3 -22) (2 18) (1 -14) (0 7))
          (sparse (3 13) (2 -21) (1 3) (0 5))]
[Entering #[compound-procedure 66 gcd-terms]
    Args: (sparse (3 13) (2 -21) (1 3) (0 5))
          (sparse (2 1458/169) (1 -2916/169) (0 1458/169))]
[Entering #[compound-procedure 66 gcd-terms]
    Args: (sparse (2 1458/169) (1 -2916/169) (0 1458/169))
          (sparse)]
[(sparse (2 1458/169) (1 -2916/169) (0 1458/169))
      <== #[compound-procedure 66 gcd-terms]
    Args: (sparse (2 1458/169) (1 -2916/169) (0 1458/169))
          (sparse)]
[(sparse (2 1458/169) (1 -2916/169) (0 1458/169))
      <== #[compound-procedure 66 gcd-terms]
    Args: (sparse (3 13) (2 -21) (1 3) (0 5))
          (sparse (2 1458/169) (1 -2916/169) (0 1458/169))]
[(sparse (2 1458/169) (1 -2916/169) (0 1458/169))
      <== #[compound-procedure 66 gcd-terms]
    Args: (sparse (4 11) (3 -22) (2 18) (1 -14) (0 7))
          (sparse (3 13) (2 -21) (1 3) (0 5))]
;Value 70: (polynomial x sparse (2 1458/169) (1 -2916/169) (0 1458/169))

;;; The problem stems from the leading terms in our polynomials not being cleanly divisible.



