###2.22###
PROMPT:
------------------------------------------------------------
Exercise 2.22.  Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

This doesn't work either. Explain.
------------------------------------------------------------

For the first procedure, the first call to iter will evaluate the alternative part of the if statement.
This statement starts at the innermost part of a list, with nil being assigned to the second part of cons.
Every call after the first will build the list out as if it were inserting a value at the beginning of the list,
hence the reverse order. The last call to iter will result in the consequent being evaluated, and then
the list is just returned in reverse order.

This bug is even more ugly. The first call to iter will again result in evaluation of the alternative.
This time, the calls to cons will start with (cons null first-value-squared). The second call (assuming there
are more than two values will be (let brackets represent a pair):

(cons [null, first-value-squared] second-value-squared)

The third call will be:

(cons [[null, first-value-squared], second-value-squared] third-value-squared)

The only way to actually use the resulting list would be to iterate it in reverse:

(define (weird-list-access items n)
  (if (= n 0)
      (cdr items)
      (weird-list-access (car items) (- n 1))))


(define (weird-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))

(weird-list-access (weird-square-list (list 1 2 3 4)) 3)
;Value: 1
(weird-list-access (weird-square-list (list 1 2 3 4)) 1)
;Value: 9

So essentially, all Louis has done is create another reversed list, but in addition
has made it even harder to work with.
      
      
												