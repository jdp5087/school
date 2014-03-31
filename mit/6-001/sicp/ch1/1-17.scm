(define remainder
  (lambda (n d)
    (/ n d)))

(define even?
  (lambda (n)
    (= (remainder n 2.0) 0)))

(define double
  (lambda (x)
    (* 2 x)))

(define half
  (lambda (x)
    (/ x 2)))

(define log-prod
  (lambda (a b)
    (if (< b 1)
	0
	(if (not (even? b))
	    (+ a (log-prod a (- b 1)))
	    (+ (double a) (log-prod a (half b)))))))



(log-prod 4 1)

(log-prod 4 5)

(log-prod 400 3000)

