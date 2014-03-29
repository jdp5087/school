(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
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

(search-for-primes-iter 10000000000000 0)

(define (search-for-primes range)
  (search-for-primes-iter range 0))


(search-for-primes 1000000000)
