(define (improve guess x)
  (/ (+ (/ x
	   (square guess))
	(* 2 guess))
     3))

(define (cbrt-good-enough? guess old-guess)
  (< (/ (abs (- guess old-guess))
	old-guess)
     0.001))


(define (cbrt-iter guess old-guess x)
  (if (cbrt-good-enough? guess old-guess)
      guess
      (cbrt-iter (improve guess x)
		 guess
		 x)))

(define (cbrt x)
  (cbrt-iter 1.0 0.001 x))

(cbrt 27)



