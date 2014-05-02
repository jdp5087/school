###2.32###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.32.  We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))
-----------------------------------------------------------------------------------------------------------------

;;; nil is not defined in my implementation of scheme so I use (list) instead
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (append (list (car s)) subset))
			   rest)))))

(subsets (list 1 2 3))
;Value 12: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(subsets (list 2 3 4 5))

This process works by taking advantage of the fact that a subset can be recursively constructed by simply adding appending each value
to a growing list of subsets. The best way to explain it is to illustrate the process in action with a very simple example.

(subsets (list 1 2))

(if (null? (1 2)))

(let (rest (subsets (2)))
  (append rest (map (lambda (subsets) (append (list 1) subset))
			   rest)))))

(let (rest (if (null? (2)))
  (append rest (map (lambda (subsets) (append (1) subset))
			   rest)))))

(let (rest
      (let ((rest (subsets nil)))
	(append rest (map (lambda (subsets) (append (list 2) subset))))))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let (rest
      (let ((rest (if (null? nil))))
	(append rest (map (lambda (subsets) (append (list 2) subset)))
			  ())))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let (rest
      (let ((rest ()))
	(append () (map (append (2) (())))))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let (rest
      (let ((rest ()))
	(append () (2)))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let (rest (append (()) ((2))))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let ((rest  (() (2))))
      (append rest (map (lambda (subsets) (append (list (1) subset))
				rest)))))

(let ((rest  (() (2))))
      (append (() (2)) (map (lambda (subsets) (append (list (1) subset))
				(() (2)))))))

(let ((rest  (() (2))))
      (append (() (2)) (map (lambda ((() (2))) (append (list (1) subset))))

(let ((rest  (() (2))))
      (append (() (2)) ((1) (1 2))

(() (2) (1) (1 2))
			
So we see that the subset of a sequence can be defined as all subsets of the sequence without the first value,
together with all subsets of the sequence without the first value with the first value appended to each subset.
This recursive definition allows us to use map to apppend the first value to each sequence in "rest" and then append
this modified rest with plain old rest. Very neat.


