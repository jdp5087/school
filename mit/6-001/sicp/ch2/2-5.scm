(define (mod b n)
  (mod-iter b n 0 0))
(define (mod-iter b n r c)
  (if (not (= 0 r))
      (- c 1)
      (mod-iter b (/ n b) (remainder n b) (+ c 1))))
(mod 2 (* (expt 2 4) (expt 3 3)))
(define (cons a b)
  (let ((p (* (expt 2 a) (expt 3 b))))
    (define deliver (lambda (b) (mod b p)))
    deliver))
(define (car m)
  ((lambda (m) (m 2)) m))
(define (cdr m)
  ((lambda (m) (m 3)) m))

(car (cons 4 5))
(cdr (cons 4 5))





