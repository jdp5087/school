###2.57###
PROMPT:
------------------------------------------------------------
Exercise 2.57.  Extend the differentiation program to handle sums and products of arbitrary numbers of (two or more) terms. Then the last example above could be expressed as

(deriv '(* x y (+ x 3)) 'x)

Try to do this by changing only the representation for sums and products, without changing the deriv procedure at all. For example, the addend of a sum would be the first term, and the augend would be the sum of the rest of the terms.
------------------------------------------------------------
(define (make-sum addend . rest)
  (let ((sum (accumulate (lambda (x y)
			   (cond ((=number? x 0) y)
				 ((and (not (null? y)) (number? x) (number? (car y))) (cons (+ x (car y)) (cdr y)))
				 (else (cons x y))))
			 '()
			 (append (list addend) rest))))
    (if (= (length sum) 1)
	(car sum)
	(cons '+ sum))))

(define (make-product multiplicand . rest)
  (define (zeros? seq)
    (define (zeros-iter seq)
      (cond ((null? seq) false)
	    ((=number? (car seq) 0) true)
	    (else (zeros-iter (cdr seq)))))
    (zeros-iter seq))
  (let ((product 
	 (accumulate (lambda (x y)
		       (cond ((or (=number? x 0) (=number? y 0)) (cons 0 '()))
			     ((=number? x 1) y)
			     ((and (not (null? y)) (number? x) (number? (car y))) (cons (* x (car y)) (cdr y)))
			     (else (cons x y))))
		     '()
		     (append (list multiplicand) rest))))
    (cond ((zeros? product) 0)
	  ((= (length product) 1) (car product))
	  (else (cons '* product)))))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cddr p))
      (cadr p)
      (apply make-product (cddr p))))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cddr s))
      (cadr s)
      (apply make-sum (cddr s))))

(deriv '(* x y (+ x 3)) 'x)
;Value 12: (+ (* x y) (* y (+ x 3)))


;;; This seemed way too difficult until I figured out that they literally meant that I was supposed
;;; to change the augend and multiplicand procedures, not just make-sum and make-product (Though as you can tell I changed those too).

