###3.14###
PROMPT:
------------------------------------------------------------
Exercise 3.14.  The following procedure is quite useful, although obscure:

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

Loop uses the ``temporary'' variable temp to hold the old value of the cdr of x, since the set-cdr! on the next line destroys the cdr. Explain what mystery does in general. Suppose v is defined by (define v (list 'a 'b 'c 'd)). Draw the box-and-pointer diagram that represents the list to which v is bound. Suppose that we now evaluate (define w (mystery v)). Draw box-and-pointer diagrams that show the structures v and w after evaluating this expression. What would be printed as the values of v and w ?
------------------------------------------------------------

;; I drew this out, and my prediction is that we will see v -> (a), and w -> (d c b a).
;; Let's find out, shall we?

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

w
;Value 16: (d c b a)

v
;Value 17: (a)

;; The reason for this is becuase v points to the beginning of the list data structure (a b c d),
;; but the first iteration of loop breaks the cdr and sets it to y, which at that point is an empty list.
;; v is set to the cdr of pairs, but the pointer v only points to the pair (a . '()).




