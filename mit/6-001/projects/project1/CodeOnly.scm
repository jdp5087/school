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

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (if (or (< vertical-velocity 0) (< elevation 0))
	false
	(let ((r1 (root1 -9.8 vertical-velocity elevation))
	      (r2 (root2 -9.8 vertical-velocity elevation)))
	  (if (> r1 r2)
	      r1
	      r2)))))

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (if (or (< vertical-velocity 0) (< elevation 0))
	false
	(let ((r1 (root1 -9.8 vertical-velocity (- elevation target-elevation)))
	      (r2 (root2 -9.8 vertical-velocity (- elevation target-elevation))))
	  (if (
	      false
	      (if (> r1 r2)
		  r1
		  r2))))))

(define (run-tests os)
  (if (string=? os "w")
      (cd "c:/school/mit/6-001/projects/project1")
      (cd "~/Documents/school/mit/6-001/projects/project1"))
  (load "../../lib.scm")
  (load "test_basebot.scm"))


(run-tests "w")



(define (complex-predicate a b)
  (if (and (boolean? a) (boolean? b))
      false
      (cond ((boolean? a) (> b 0))
	    ((boolean? b) (> a 0))
	    ((and (< 0 a) (< 0 b)) 
	  




