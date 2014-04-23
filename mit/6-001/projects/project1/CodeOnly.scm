(define square
  (lambda (x) (* x x)))

(define position
  (lambda (a v u t)
    (+ (* (/ 1 2) a (square t))
       (* v t)
       u)))

(define (root1 a b c)
  (define quad-radical-algebra
    (lambda (a b c)
      (- (square b) (* 4 a c))))
  (define quad-radical
    (lambda (a b c)
      (sqrt (quad-radical-algebra a b c))))
  (define quad-flex
    (lambda (a b c sign)
      (/ (sign (- b) (quad-radical a b c))
	 (* 2 a))))
    (if (or (= a 0) (< (quad-radical-algebra a b c) 0))
	false
	(quad-flex a b c +)))

(define (root2 a b c)
  (define quad-radical-algebra
    (lambda (a b c)
      (- (square b) (* 4 a c))))
  (define quad-radical
    (lambda (a b c)
      (sqrt (quad-radical-algebra a b c))))
  (define quad-flex
    (lambda (a b c sign)
      (/ (sign (- b) (quad-radical a b c))
	 (* 2 a))))
    (if (or (= a 0) (< (quad-radical-algebra a b c) 0))
	false
	(quad-flex a b c -)))


(define time-to-impact
  (lambda (vertical-velocity elevation)
    (let ((r1 (root1 -9.8 vertical-velocity elevation))
	  (r2 (root2 -9.8 vertical-velocity elevation)))
      (if (> r1 r2)
	  r1
	  r2))))

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (let ((r1 (root1 -9.8 vertical-velocity (- elevation target-elevation)))
	  (r2 (root2 -9.8 vertical-velocity (- elevation target-elevation))))
      (if (> r1 r2)
	  r1
	  r2))))



(define (test v1 v2)
  (define tolerance 0.0001)
  (if (< (abs (- v1 v2)) tolerance)
      (display "Test passed\n")
      (display 



(let ((v1 8)
      (v2 9))
  (display v1 v2))