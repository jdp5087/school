###1.34###
PROMPT:
------------------------------------------------------------
Exercise 1.34.  Suppose we define the procedure

(define (f g)
  (g 2))

Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1))))
6

What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
------------------------------------------------------------

Before actually checking this, I'll  just use the substitution model to answer this question. It looks like the interpreter will end up trying to apply the object 2 to 2, so we should get an "object is not applicable" error.

(f f)
(f 2)
(2 2)

lets test this result, shall we?

(define f
  (lambda (g) (g 2)))

(f (lambda (x) (* x x)))

(f (lambda (z) (* z (+ z 1))))

(f f)
;The object 2 is not applicable.

Ah HA! The reason I knew that was coming is because I write code with lots of bugs, and I've seen that error previously.

