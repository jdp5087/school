###2.39###
PROMPT:
------------------------------------------------------------
Exercise 2.39.   Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and fold-left from exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) nil sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) <??>) nil sequence))
------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(reverse (list 1 2 3))
;Value 19: (3 2 1)


(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

(reverse (list 1 2 3))
;Value 23: (3 2 1)

;;; I confess that I had to check online for the answer to the fold-right case, which gave me enough to figure out
;;; the fold-left case pretty easily.
