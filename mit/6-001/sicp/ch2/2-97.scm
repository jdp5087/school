###2.97###
-----------------------------------------------------------------------------------------------------------------
Exercise 2.97.  a. Implement this algorithm as a procedure reduce-terms that takes two term lists n and d as arguments and returns a list nn, dd, which are n and d reduced to lowest terms via the algorithm given above. Also write a procedure reduce-poly, analogous to add-poly, that checks to see if the two polys have the same variable. If so, reduce-poly strips off the variable and passes the problem to reduce-terms, then reattaches the variable to the two term lists supplied by reduce-terms.

b. Define a procedure analogous to reduce-terms that does what the original make-rat did for integers:

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

and define reduce as a generic operation that calls apply-generic to dispatch to either reduce-poly (for polynomial arguments) or reduce-integers (for scheme-number arguments). You can now easily make the rational-arithmetic package reduce fractions to lowest terms by having make-rat call reduce before combining the given numerator and denominator to form a rational number. The system now handles rational expressions in either integers or polynomials. To test your program, try the example at the beginning of this extended exercise:

(define p1 (make-polynomial 'x '((1 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add rf1 rf2)

See if you get the correct answer, correctly reduced to lowest terms.
-----------------------------------------------------------------------------------------------------------------

;; Here are the procedures I wrote in the polynomial packages. The rest are just generic operations, or
;; the addition of a call to reduce in make-rat, please see poly3.scm and coercion-2-86.scm for details

;; One note here, I know that all of the calls to let are rather ugly, but I have yet to decide whether it
;; is better to increase complexity by having to pass values between multiple functions, or to have ugly
;; syntax with let statements. I'm leaning towards the let statements because they are probably less prone towards
;; bugs.

(define (reduce-terms n d)
  (let ((g (gcd-terms n d)))
    (let ((c (coeff (first-term g)))
	  (o2 (order (first-term g)))
	  (n-o (order (first-term n)))
	  (d-o (order (first-term d))))
      (let ((o1 (if (> n-o d-o) n-o d-o)))
	(let ((int-coeff (expt c (- (+ 1 o1) o2))))
	  (let ((int-term (make-term 0 int-coeff)))
	    (let ((reduced-n (car (div-terms (mul-term-by-all-terms int-term n) g)))
		  (reduced-d (car (div-terms (mul-term-by-all-terms int-term d) g))))
	      (let ((gcd-coeffs (find-total-gcd (append (extract-coeffs reduced-n)
							(extract-coeffs reduced-d)))))
		(list (car (div-terms reduced-n (adjoin-term (make-term 0 gcd-coeffs)
							     (the-empty-termlist 'sparse))))
		      (car (div-terms reduced-d (adjoin-term (make-term 0 gcd-coeffs)
							     (the-empty-termlist 'sparse)))))))))))))

(define (reduce-poly n d)
  (let ((v1 (variable n))
	(v2 (variable d)))
    (if (not (eq? v1 v2))
	(error "numerator and denominator not in the same var -- REDUCE-POLY -- " (list n d))
	(let ((reduced-termlists (reduce-terms (term-list n) (term-list d))))
	  (list (make-poly v1 (car reduced-termlists))
		(make-poly v2 (cadr reduced-termlists)))))))


(define p1 (make-polynomial 'x '(sparse (1 1) (0 1))))
(define p2 (make-polynomial 'x '(sparse (3 1) (0 -1))))
(define p3 (make-polynomial 'x '(sparse (1 1))))
(define p4 (make-polynomial 'x '(sparse (2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

rf1
rf2
(add rf1 rf2)
;Value 17: (rational (polynomial x sparse (3 -1) (2 -2) (1 -3) (0 -1)) polynomial x sparse (4 -1) (3 -1) (1 1) (0 1))

;; And that's a wrap on chapter 2. That was a LONG one.