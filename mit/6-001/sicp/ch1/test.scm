


(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt x)
  (sqrt-iter 1.0 0.001 x))

(define (new-good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess))
	old-guess)
     0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess old-guess x)
  (if (new-good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x)
		 guess
                 x)))


(sqrt 4)




