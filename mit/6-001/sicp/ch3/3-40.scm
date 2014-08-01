###3.40###
PROMPT:
------------------------------------------------------------
Exercise 3.40.  Give all possible values of x that can result from executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

Which of these possibilities remain if we instead use serialized procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
------------------------------------------------------------

We have 5 accesses to x, a multiplication of x*x*x, a multiplication of x*x, and 2 sets.

However, let the sequence of execution of the first procedure be get1, get2, mul1, and
set1, and the sequence of execution of the second procedure be get3, get4, get5, mul2, and
set2. Now there is only one possible ordering of the first procedure (within itself), and
one possible ordering for the second procedure. Therefore, there exists a mapping between
the possible orderings of interleaved procedures and a string of 9 characters with
4 1's and 5 0's. Therefore, the number of possible different orderings of procedures is
9!/(5!4!) = 126 orderings. However, the only orderings that have significance are the ones
where a set occurs before the other procedure has finished accessing the value of x, or which
set precedes the other.
