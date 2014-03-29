(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))
      0))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  1)


(define (search-for-primes-iter range found)
  (if (>= found 3)
      (finish-up)
      (run-timed-prime-test range found)))

(define (finish-up)
  (newline)
  (display "All Done!"))

(define (run-timed-prime-test n f)
  (search-for-primes-iter
   (+ n 1)
   (+ (timed-prime-test n) f)))

(define (search-for-primes range)
  (search-for-primes-iter range 0))

(search-for-primes 1000000000)
(search-for-primes 1000000000000)
(search-for-primes 1000000000000000)
(search-for-primes 1000000000000000000)
(search-for-primes 1000000000000000000000)
(search-for-primes 100000000000000000000000000000000000000000000000000000000000000000000000000)
