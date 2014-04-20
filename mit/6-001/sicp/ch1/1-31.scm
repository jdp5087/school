###1.31###
PROMPT:
------------------------------------------------------------
Exercise 1.31.   
a.  The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures.51 Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to  using the formula52


b.  If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
------------------------------------------------------------

(define (product a b term next)
  (if (> a b)
      1
      (* (term a)
	 (product (next a) b term next))))

(define (product-iter a b term next check compare)
  (define (iter a result)
    (if (compare (check a) (check b))
	result
	(iter (next a) (* result (term a)))))
  (iter a 1.0))

(define (factorial n)
  (define (identity x) x)
  (define (fact-next a)
    (+ a 1))
  (product-iter 1 n identity fact-next))

(factorial 5)


(define (pi-approx start-pair stop-pair)
  (define (pi-term a)
    (/ (car a) (cdr a)))
  (define (pi-next a)
    (if (> (car a) (cdr a))
	(cons (car a) (+ (cdr a) 2.0))
	(cons (+ (car a) 2.0) (cdr a))))
  (product-iter start-pair stop-pair pi-term pi-next pi-term =))

(* 4 (pi-approx (cons 2.0 3.0) (cons 1000.0 1001.0)))
