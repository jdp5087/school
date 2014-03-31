(define (expmod base exp m one-mod-n)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (check-one-mod-n (square (expmod base (/ exp 2) m one-mod-n)) one-mod-n)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m one-mod-n))
                    m))))
(define (check-one-mod-n sqr one-m-n)
  (if (and (= sqr one-m-n) (not (or (= sqr 1) (= sqr (- m 1)))))
      0
      sqr))
(define (miller-rabin n one-mod-n)
  (define (try-it a)
    (= (expmod a (- n 1) n one-mod-n) one-mod-n))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin n (remainder 1 n)) (fast-prime? n (- times 1)))
	(else false)))

(fast-prime? 3 5)
;Value: #t

(fast-prime? 199 5)
;Value: #t


(fast-prime? 561 5)
;Value: #f

(fast-prime? 1105 5)
;Value: #f

(fast-prime? 1729 5)
;Value: #f

(fast-prime? 2465 5)
;Value: #f

(fast-prime? 2821 5)
;Value: #f

(fast-prime? 6601 5)
;Value: #f
