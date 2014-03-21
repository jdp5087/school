;;; Part 1

;;; Jon Poler
;;; Project 0


;;; Part 2

-37
;Value: -37

(> 10 9.7)
;Value: #t


(- (if (> 3 4)
       7
       10)
   (/ 16 10))
;Value: 42/5

(* (- 25 10)
   (+ 6 3))
;Value: 135

+
;Value 11: #[arity-dispatched-procedure 11]

(define double (lambda (x) (* 2 x)))
;Value: double

double
;Value 15: #[compound-procedure 15 double]

(define c 4)
;Value: c

c
;Value: 4

(double (double (+ c 5)))
;Value: 36

(define times-2 double)
;Value: times-2

(times-2 c)
;Value: 8

(define d c)
;Value: d

(= c d)
;Value: #t

(cond ((>= c 2) d)
      ((= c (- d 5)) (+ c d))
      (else (abs (- c d))))
;Value: 4

;;; Part 3

(define abs
  (lambda (a)
    (if (> a 0)
	a
	(- a))))
;Value: abs


(define (cube x)
  (if (not (number? x))
      (error "Argument should be a number instead of" x)
      (* x x x)))
;Value: cube

;(cube 'hi)

(abs 0)

;;; Part 5

;1. The debugger kicks in after an error, the stepper is invoked
;   voluntarily

;2. Can't find "Guide to MIT Scheme"

;3. N/A

;4. N/A

;5. N/A

;6. Three methods for controlling complexity:

;    a. Procedure and data abstraction
;    b. Conventional interfaces and programming paradigms
;    c. Metalinguistic abstraction

(and (= 1 1) (< 1 2))