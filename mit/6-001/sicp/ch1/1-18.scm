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

(define log-prod-iter
  (lambda (a b total)
    (if (< b 1)
	total
	(if (not (even? b))
	    (log-prod-iter a (- b 1) (+ total a))
	    (log-prod-iter a (half b)(double a))))))

(define log-prod
  (lambda (a b)
    (log-prod-iter a b 0)))

(log-prod 0 4)

(log-prod 4 0)

(log-prod 1 4)

(log-prod 4 1)

(log-prod 4 5)

(log-prod 400 3000)

