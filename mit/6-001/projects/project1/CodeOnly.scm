;;(cd "~/Documents/school/mit/6-001/projects/project1")
;;(cd "c:/school/mit/6-001/projects/project1")
;;(load "../../lib.scm")


(define square
  (lambda (x) (* x x)))

(define position
  (lambda (a v u t)
    (+ (* (/ 1 2) a (square t))
       (* v t)
       u)))

(define (root-flex a b c sign)
  (define (radical-inner)
    (- (square b) (* 4 a c)))
  (define (radical)
    (sqrt (radical-inner)))
  (define (quad)
    (/ (sign (- b) (radical))
       (* 2 a)))
  (let ((rad (radical-inner)))
    (if (or (= a 0) (< rad 0))
	false
	(quad))))

(define (root1 a b c)
  (root-flex a b c -))

(define (root2 a b c)
  (root-flex a b c +))

(define (time-to-flex vertical-velocity elevation target-elevation predicate height-func)
  (if (or (< vertical-velocity 0) (< elevation 0))
      false
      (let ((r1 (root1 (/ -9.8 2) vertical-velocity (height-func elevation target-elevation)))
	    (r2 (root2 (/ -9.8 2) vertical-velocity (height-func elevation target-elevation))))
	(if (predicate r1 r2)
	    (if (> r1 r2)
		r1
		r2)
	    false))))

(define (complex-predicate a b)
      (if (and (boolean? a) (boolean? b))
	  false
	  (cond ((boolean? a) (> b 0))
		((boolean? b) (> a 0))
		(else (and (>= a 0) (>= a 0))))))

(define (time-to-impact vertical-velocity elevation)
  (time-to-flex vertical-velocity
		elevation
		false
		complex-predicate
		(lambda (x y) x)))

(define (time-to-height vertical-velocity elevation target-elevation)
  (time-to-flex vertical-velocity
		elevation
		target-elevation
		complex-predicate
		(lambda (x y) (- x y))))

(define (product-iter a b term next check compare)
  (define (iter a result)
    (if (compare (check a) (check b))
	result
	(iter (next a) (* result (term a)))))
  (iter a 1.0))

(define (pi-approx start-pair stop-pair)
  (define (pi-term a)
    (/ (car a) (cdr a)))
  (define (pi-next a)
    (if (> (car a) (cdr a))
	(cons (car a) (+ (cdr a) 2.0))
	(cons (+ (car a) 2.0) (cdr a))))
  (product-iter start-pair stop-pair pi-term pi-next pi-term =))

(define pi (* 4 (pi-approx (cons 2.0 3.0) (cons 1000.0 1001.0))))


(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

(define (mph-to-mps rate)
  (/ (feet-to-meters (* rate 5280)) (hours-to-seconds 1)))

(define (mps-to-mph rate)
  (/ (/ (meters-to-feet rate) 5280) (seconds-to-hours 1)))

(define (travel-distance-simple velocity angle elevation)
  (define (distance-horizontal v a t)
    (* v (cos a) t))
  (let ((e (feet-to-meters elevation))
	(v (mph-to-mps velocity))
	(a (degree2radian angle)))
    (let ((t (time-to-impact (* (sin a) v) e)))
      (distance-horizontal v a t))))

(travel-distance-simple 100 45 3)

(define (find-best-angle velocity elevation)
  (define (greater-angle current-distance best-distance current-angle best-angle)
    (if (> current-distance best-distance)
	current-angle
	best-angle))
  (define (greater-distance current-distance best-distance)
    (if (> current-distance best-distance)
	current-distance
	best-distance))
  (define (find-best-angle-iter velocity angle elevation best-angle best-distance)
    (if (> angle 90)
	best-angle
	(let ((distance (travel-distance-simple velocity angle elevation)))
	  (find-best-angle-iter velocity (+ angle 1) elevation (greater-angle distance best-distance angle best-angle) (greater-distance distance best-distance)))))
  (find-best-angle-iter velocity 0 elevation 0 0))

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define (integrate x0 y0 u0 v0 dt g m beta)
  (let ((x-tot 0)
	(y-tot h)
	(u-tot u0)
	(v-tot v0))
    (define (integrate-iter x y u v)
      (if (< 0 y)
	  x?
    





    
    


(define (run-tests os)
  (if (string=? os "w")
      (cd "c:/school/mit/6-001/projects/project1")
      (cd "~/Documents/school/mit/6-001/projects/project1"))
  (load "../../lib.scm")
  (load "test_basebot.scm"))


(run-tests "w")







