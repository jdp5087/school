###2.37###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.37.  Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as sequences of vectors (the rows of the matrix). For example, the matrix


is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:


We can define the dot product

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

(define (matrix-*-vector m v)
  (map <??> m))
(define (transpose mat)
  (accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <??> m)))
-----------------------------------------------------------------------------------------------------------------

(define basic-m (list (list 1 2) (list 1 2)))
(define basic-n (list (list 1 2) (list 1 2)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (seq) (dot-product v seq)) m))

(matrix-*-vector m (list 1 2 3 4))

(define (transpose m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
	   (accumulate (lambda (n-col n-rest)
			 (cons (dot-product m-row n-col)
			       n-rest))
		       (list)
		       cols))
	 m)))

(matrix-*-matrix m n)
;Value 14: ((70 80 90) (126 147 168) (180 210 240))

(matrix-*-matrix basic-m basic-n)
;Value 15: ((3 6) (3 6))
