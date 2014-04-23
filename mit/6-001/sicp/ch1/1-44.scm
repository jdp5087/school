###1.44###
PROMPT:
------------------------------------------------------------
Exercise 1.44.  The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.
------------------------------------------------------------

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))


(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (square x) (* x x))
(define (cube x) (* x x x))

((smooth (smooth square)) 5)
		       
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((smooth (smooth (smooth square))) 5)
;Value: 25.0000000002

((n-fold-smooth square 3) 5)
;Value: 125.00000000299997

((smooth (smooth (smooth cube))) 5)
((n-fold-smooth cube 3) 5)
;Value: 125.00000000299997
