(define square
  (lambda (x)
    (* x x)))

(define remainder
  (lambda (n d)
    (/ n d)))

(define even?
  (lambda (n)
    (= (remainder n 2.0) 0)))

(define fast-exp
  (lambda (b n)
    (fast-exp-iter b n 1)))

(define fast-exp-iter
  (lambda (b n a)
    (if (< n 1)
	a
	(if (even? n)
	    (fast-exp-iter b (/ n 2) (* a (square b)))
	    (fast-exp-iter b (- n 1) (* a b))))))

(fast-exp 2 17)
