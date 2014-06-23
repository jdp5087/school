###2.94###
-----------------------------------------------------------------------------------------------------------------
Exercise 2.94.  Using div-terms, implement the procedure remainder-terms and use this to define gcd-terms as above. Now write a procedure gcd-poly that computes the polynomial GCD of two polys. (The procedure should signal an error if the two polys are not in the same variable.) Install in the system a generic operation greatest-common-divisor that reduces to gcd-poly for polynomials and to ordinary gcd for ordinary numbers. As a test, try

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

and check your result by hand.
-----------------------------------------------------------------------------------------------------------------

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))

;;; FOLLOWING PROCEDURES ADDED TO INSTALL-POLYNOMIAL PACKAGE
(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (not (equal? (variable p1) (variable p2)))
      (error "polys not in same var -- GCD-POLY -- " (list (variable p1) (variable p2)))
      (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))))

(put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))


;; Very straightforward. These last few exercises are moving at lightning speed when compared with 2.90 and 2.92.




