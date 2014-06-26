###3.13###
PROMPT:
------------------------------------------------------------
Exercise 3.13.  Consider the following make-cycle procedure, which uses the last-pair procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (list 'a 'b 'c)))

What happens if we try to compute (last-pair z)?
------------------------------------------------------------

An infinite loop, I also drew this out on paper. The reason is because there is no longer a null list to act as a sentinel.


