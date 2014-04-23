###1.38###
PROMPT:
------------------------------------------------------------
Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.
------------------------------------------------------------

(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
	result
	(cont-frac-iter (- i 1)
			(/ (n i)
			   (+ (d i) result)))))
  (cont-frac-iter k 0))

(define (eulers-e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
	     (lambda (i) 
	       (define (d-iter a b c count)
		 (if (> count i)
		     c
		     (if (not (= a 1))
			 (d-iter b c (+ a b c) (+ count 1))
			 (d-iter b c 1 (+ count 1)))))
	       (d-iter 1 0 1 1))
	     k)))

(eulers-e 1)
;Value: 3.
(eulers-e 5)
;Value: 2.71875
(eulers-e 10)
;Value: 2.7182817182817183
(eulers-e 20)
;Value: 2.718281828459045

;;; Note here that the d function is relatively inefficient because it calculates the d from 1-i on every iteration.
;;; A function that stores the history of the last three values returned would be much more efficient.
