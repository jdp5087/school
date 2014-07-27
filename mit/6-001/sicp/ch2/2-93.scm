###2.93###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.93.  Modify the rational-arithmetic package to use generic operations, but change make-rat so that it does not attempt to reduce fractions to lowest terms. Test your system by calling make-rational on two polynomials to produce a rational function

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rf (make-rational p2 p1))

Now add rf to itself, using add. You will observe that this addition procedure does not reduce fractions to lowest terms.
-----------------------------------------------------------------------------------------------------------------

;; Please see coercion-2-86.scm for the code here. The only hiccup was that I had to write a procedure to test for rationals that have a
;; polynomial in either their numerator or denominator.

;; This procedure was then used within apply-drop inside apply-generic, to prevent the algorithm from attempting to drop a rational function,
;; because coercing a polynomial downwards to an integer is nonsense (we'll assume that we arent going to make a polynomial
;; just to store an integer for the sake of simplicity, though it should be recognized as a possibility).

;; All of the changes to implement generic procedures within the rational package can be seen within the code.

(define (rat-function? obj)
  (if (not (equal? (type-tag obj) 'rational))
      false
      (let ((n (numer obj))q
	    (d (denom obj)))
	(if (not (or (poly? n) (poly? d)))
	    false
	    true))))

