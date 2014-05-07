###2.41###
PROMPT:
------------------------------------------------------------
Exercise 2.41.  Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than or equal to a given integer n that sum to a given integer s.
------------------------------------------------------------

(define (unique-triplets n)
  (flatmap (lambda (x)
	     (flatmap (lambda (y)
		    (map (lambda (z)
			   (list z y x))
			 (enumerate-sequence 1 (- y 1))))
		  (enumerate-sequence 1 (- x 1))))
	   (enumerate-sequence 1 n)))

(unique-triplets 5)
;Value 32: ((1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 5) (1 3 5) (2 3 5) (1 4 5) (2 4 5) (3 4 5))


(define (triplets-sum n s)
  (define (sum-to-s? triplet)
    (= s (accumulate + 0 triplet)))
  (filter sum-to-s? (unique-triplets n)))

(triplets-sum 5 9)
