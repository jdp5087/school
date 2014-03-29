(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

;;; 1 call to remainder, 1 total

(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

;;; 2 calls to remainder, 3 total.

(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	 (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

;;; 4 calls to remainder, 7 total

(if (= 2 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	 (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
		(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 40 (remainder 206 40))
		  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
       0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	 (remainder
	  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	  (remainder (remainder 40 (remainder 206 40))
		     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;;; 7 calls to remainder, 14 total.

(if (= 0 0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
		    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
	 (remainder
	  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
	  (remainder (remainder 40 (remainder 206 40))
		     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

;;; 4 calls to remainder, 18 total.

This was using the normal-order evaluation model. In applicative order evaluation, the remainder operation would be applied to the operands
before being substituted in the procedure body as parameters. Lets see what that looks like.

(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40)))

;;; 1 call to remainder, 1 total

(gcd 40 6)

(if (= 6 0)
    6
    (gcd 6 (remainder 40 6)))

(gcd 6 (remainder 40 6)))

;;; 1 call to remainder, 2 total

(gcd 6 4)

(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

(gcd 4 (remainder 6 4))

;;; 1 call to remainder, 3 total

(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

(gcd 2 (remainder 4 2))

;;; 1 call to remainder, 4 total

(gcd 2 0)

(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

2

So we compare 18 calls to remainder in normal-order-evaluation, and 4 calls to remainder in applicative-order-evaluation.

###1.21###

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))


(smallest-divisor 199)
;Value: 199

(smallest-divisor 1999)
;Value: 1999

(smallest-divisor 19999)
;Value: 7

###1.22###










