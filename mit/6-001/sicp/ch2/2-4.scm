(define (cons x y)
  (lambda (m) (m x y)))

(define (car m)
  (z (lambda (p q) p)))

***Substitution model***

(car (cons x y))
(car (lambda (m) (m x y)))
(car #[compound-procedure 12])
(#[compound-procedure 12] (lambda (p q) p))
(#[compound-procedure 12] #[compound-procedure 13])
---for the sake of clarity ill write out the actual procedures)
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
((lambda (x y) x) x y) ;;;<---which is like saying
x

So, the corresponding definition of cdr is as follows:

(define (cdr z)
  (z (lambda (p q) q)))
 




      
