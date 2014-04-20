###1.33###
------------------------------------------------------------
Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
------------------------------------------------------------

(define (accumulate-filter combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
	result
	(if (predicate a)
	    (iter (next a) (combiner result (term a)))
	    (iter (next a) result))))
  (iter a null-value))

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


(define (prime? n)
  (fast-prime? n 5))

(define (sum-filter term a next b predicate)
  (accumulate-filter + 0 term a next b predicate))

(define (product-filter term a next b predicate)
  (accumulate-filter * 1 term a next b predicate))

(define (sum-prime-squares a b)
  (define (square x) (* x x))
  (define (next a) (+ a 1))
  (sum-filter square a next b prime?))

(sum-prime-squares 2 10)
;Value: 87

(define (product-rel-primes n)
  (define (gcd x y)
    (if (= (remainder x y) 0)
	y
	(gcd y (remainder x y))))
  (define (pred a)
    (= (gcd n a) 1))
  (define (identity a) a)
  (define (next a) (+ a 1))
  (product-filter identity 1 next (- n 1) pred))

(product-rel-primes 10)
;Value: 189


