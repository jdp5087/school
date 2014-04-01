I found this to be highly confusing, so I just followed the hint to see if it would provide any insight.
I'm merely using the substitution model to decipher what is going on here.

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



(((lambda (f) (lambda (x) (f x)))) f) x)))
(lambda (x) (x x))))


(lambda (f)
  (lambda (x)
    (f (((lambda (f)
	   (lambda (x)
	     (f x)))) f) x)))

(lambda (f)
  (lambda (x)
    (f ((lambda (x)
	     (x x)))) x)))

(lambda (f)
  (lambda (x)
    (f (x x))))


