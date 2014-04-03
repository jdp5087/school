Heres the prompt:

--------------------------------------------------------------------------------
Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the  calculus.

Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1).
--------------------------------------------------------------------------------

So let's just follow the hint of using the substitution model and see if we can figure out what in the world is going on here.

(add-1 zero)

(add-1 (lambda (f) (lambda (x) x)))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x)) x))))
(lambda (f) (lambda (x) (f x)))) <<<-----direct definition of one

(add-1 (add-1 zero))
(add-1 (add-1 (lambda (f) (lambda (x) x))))
(add-1 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f x)))))
(add-1 (lambda (f) (lambda (x) (f ((lambda (x) x)) x))))
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x)))) f) x)))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)))) x)))
(lambda (f) (lambda (x) (f (f x)))) <<<-----direct definition of two


(define (+ n1 n2)
  (lambda (f) (lambda (x) ((n1 f) ((n2 f) x))))) <<<-----direct definition of add

(+ (lambda (f) (lambda (x) (f (f x)))) (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f x)))) f) (f x))))
(lambda (f) (lambda (x) ((lambda (x) (f (f x))) (f x))))
(lambda (f) (lambda (x) (f (f (f x)))))

While this has been an eye opening experience, the usefulness here doesn't exactly blow me away. One of the huge aspects of good code is that
it should be readable. I spent 2 or so hours trying to think through all of the twists and contortions that the procedures are doing here.
While it is elegant, the practical use is not apparent to me. When I was trying to read the code, I found that telling myself "Well, this is a procedure, that takes f
and returns a procedure that takes x, which applies f with x substituted in to a composition of n and f" to be entirely confusing and inadequate for
understanding what is going on. While eventually I did grasp the idea here, it wasn't without struggle. perhaps the lesson I learned is to never write code
like this if I plan on having anyone else read it.