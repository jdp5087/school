(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (congruent? n)
  (congruent-alternate n 1))

(define (congruent-iter n a)
  (cond ((= n a) true)
	((= (expmod a n n) (remainder a n)) (congruent-iter n (+ a 1)))
	(else false)))

(define (congruent-alternate n a)
  (cond ((= n a) true)
	((= 0 (remainder (- (fast-expt a n) a) n)) (congruent-alternate n (+ a 1)))
	(else false)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


(congruent? 2)

(congruent? 561)
;Value: #t

(congruent? 1105)
;Value: #t

(congruent? 1729)
;Value: #t

(congruent? 2465)
;Value: #t

(congruent? 2821)
;Value: #t

(congruent? 6601)
;Value: #t

(congruent? 6602)
;Value: #f

