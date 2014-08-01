###3.39###
PROMPT:
------------------------------------------------------------
Exercise 3.39.  Which of the five possibilities in the parallel execution shown above remain if we instead serialize execution as follows:

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
------------------------------------------------------------

;; Because the setting of x is not serialized along with the lambda function to compute a square,
;; the sequence

;; 100: P1 accesses x (twice), then P2 sets x to 11, then p1 sets x

;; is still possible.
