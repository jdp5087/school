###2.40###
PROMPT:
------------------------------------------------------------
Exercise 2.40.  Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j) with 1< j< i< n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.
------------------------------------------------------------

(define (enumerate-sequence low high)
  (if (> low high)
      ()
      (cons low (enumerate-sequence (+ 1 low) high))))

(enumerate-sequence 1 20)

(define (flatmap proc sequence)
  (accumulate append () (map proc sequence)))

(define (unique-pairs n)
  (flatmap (lambda (x)
	     (map (lambda (y) (cons y x))
		  (enumerate-sequence 1 (- x 1))))
	     (enumerate-sequence 1 n)))

(unique-pairs 5)
;Value 27: ((1 . 2) (1 . 3) (2 . 3) (1 . 4) (2 . 4) (3 . 4) (1 . 5) (2 . 5) (3 . 5) (4 . 5))

		  
				  
