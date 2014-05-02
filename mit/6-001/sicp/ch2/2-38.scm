###2.38###
PROMPT:
------------------------------------------------------------
Exercise 2.38.  The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

What are the values of

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.
------------------------------------------------------------

(define (fold-right op init sequence)
  (accumulate op init sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;;; Should be 3/2
;Value: 3/2

(fold-left / 1 (list 1 2 3))
;;; Should be 1/6
;Value: 1/6

(fold-right list (list) (list 1 2 3))
;;; Should be (1 (2 (3 (list))))
;Value 17: (1 (2 (3 ())))

(fold-left list (list) (list 1 2 3))
;;; Should be ((((list) 1) 2) 3))
;Value 18: (((() 1) 2) 3)

The operator cannot be sensitive to order. For instance, multiplication and addition meet this criterion,
while addition and subtraction do not.

(fold-right + 0 (list 1 2 3))
;Value: 6
(fold-left + 0 (list 1 2 3))
;Value: 6

(fold-right - 0 (list 1 2 3))
;Value: 2
(fold-left - 0 (list 1 2 3))
;Value: -6
