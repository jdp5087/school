###1.46###
PROMPT:
------------------------------------------------------------
Exercise 1.46.  Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 1.3.3 in terms of iterative-improve.
------------------------------------------------------------

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
(define (iterative-improve improve good-enough?)
  (define (try guess)
    (let ((next (improve guess)))
      (if (good-enough? next guess)
	  next
	  (try next))))
  try)

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve f (lambda (v1 v2) (< (abs (- v1 v2))
					   tolerance)))
   first-guess))

(define (sqrt x)
  (define (average-damp x y)
    (lambda (y) (/ (+ x y) 2)))
  (fixed-point (average-damp x (lambda (y) (/ x y)))
	       1.1))

(fixed-point cos 1.0)
;Value: .7390822985224024
(sqrt 4)
;Value: 3.999994468688965
