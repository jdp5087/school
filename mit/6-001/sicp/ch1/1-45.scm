###1.45###
------------------------------------------------------------
Exercise 1.45.  We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y  x/y2. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y  x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y  x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y  x/yn-1. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.
------------------------------------------------------------

(define (average-damp f)
  (define (average x y)
    (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

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

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(define (naive-sqrt x)
  (fixed-point (lambda (y) (/ x y))
	       1.0))

(naive-sqrt 4)
;;; Doesn't converge (obviously)


;;; This was part of my experiment, better implementation below.
(define (nth-root x n damps)
  (fixed-point ((repeated average-damp damps) (lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

(nth-root 256 64 6)

;;; Requires log(n) damps for 2, make sure to test other numbers


(define (lg n b)
  (define (lg-iter n count)
    (if (< n b)
	count
	(lg-iter (/ n b) (+ count 1))))
  (lg-iter n 0))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (lg n 2)) (lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

(nth-root 256 64)
;;; Converges up to n = 200 (I didn't bother checking past that)
