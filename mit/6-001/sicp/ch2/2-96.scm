###2.96###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.96.  a.    Implement the procedure pseudoremainder-terms, which is just like remainder-terms except that it multiplies the dividend by the integerizing factor described above before calling div-terms. Modify gcd-terms to use pseudoremainder-terms, and verify that greatest-common-divisor now produces an answer with integer coefficients on the example in exercise 2.95.

b.    The GCD now has integer coefficients, but they are larger than those of P1. Modify gcd-terms so that it removes common factors from the coefficients of the answer by dividing all the coefficients by their (integer) greatest common divisor.
-----------------------------------------------------------------------------------------------------------------

;;; Procedures written for 2.96

  (define (pseudoremainder-terms a b)
    (let ((order-a (order (first-term a)))
	  (order-b (order (first-term b)))
	  (coeff-b (coeff (first-term b))))
      (let ((integerizing-factor (expt coeff-b (- (+ 1 order-a) order-b))))
	(cadr (div-terms (mul-term-by-all-terms (make-term 0 integerizing-factor)
						a)
			 b)))))
  (define (extract-coeffs terms)
    (if (empty-termlist? terms)
	'()
	(cons (coeff (first-term terms))
	      (extract-coeffs (rest-terms terms)))))

  (define (find-total-gcd a)
    (let ((last-term (car a)))
      (let ((all-terms (append (list last-term) (reverse a))))
	(reduce-right gcd false all-terms))))

  (define (div-coeffs-by-constant terms constant)
    (car (div-terms terms
		    (adjoin-term (make-term 0 constant)
				 (the-empty-termlist 'sparse)))))
	  

  (define (gcd-terms a b)
    (if (empty-termlist? b)
	(div-coeffs-by-constant a (find-total-gcd (extract-coeffs a)))
	(gcd-terms b (pseudoremainder-terms a b))))

;; Result after implementing part a
[Entering #[compound-procedure 71 gcd-terms]
    Args: (sparse (4 11) (3 -22) (2 18) (1 -14) (0 7))
          (sparse (3 13) (2 -21) (1 3) (0 5))]
[Entering #[compound-procedure 71 gcd-terms]
    Args: (sparse (3 13) (2 -21) (1 3) (0 5))
          (sparse (2 1458) (1 -2916) (0 1458))]
[Entering #[compound-procedure 71 gcd-terms]
    Args: (sparse (2 1458) (1 -2916) (0 1458))
          (sparse)]
[(sparse (2 1458) (1 -2916) (0 1458))
      <== #[compound-procedure 71 gcd-terms]
    Args: (sparse (2 1458) (1 -2916) (0 1458))
          (sparse)]
[(sparse (2 1458) (1 -2916) (0 1458))
      <== #[compound-procedure 71 gcd-terms]
    Args: (sparse (3 13) (2 -21) (1 3) (0 5))
          (sparse (2 1458) (1 -2916) (0 1458))]
[(sparse (2 1458) (1 -2916) (0 1458))
      <== #[compound-procedure 71 gcd-terms]
    Args: (sparse (4 11) (3 -22) (2 18) (1 -14) (0 7))
          (sparse (3 13) (2 -21) (1 3) (0 5))]
;Value 72: (polynomial x sparse (2 1458) (1 -2916) (0 1458))

;; Result after implementing part b
;Value 83: (polynomial x sparse (2 1) (1 -2) (0 1))

