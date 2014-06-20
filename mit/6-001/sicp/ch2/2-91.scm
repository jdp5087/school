###2.91###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.91.  A univariate polynomial can be divided by another one to produce a polynomial quotient and a polynomial remainder. For example,


Division can be performed via long division. That is, divide the highest-order term of the dividend by the highest-order term of the divisor. The result is the first term of the quotient. Next, multiply the result by the divisor, subtract that from the dividend, and produce the rest of the answer by recursively dividing the difference by the divisor. Stop when the order of the divisor exceeds the order of the dividend and declare the dividend to be the remainder. Also, if the dividend ever becomes zero, return zero as both quotient and remainder.

We can design a div-poly procedure on the model of add-poly and mul-poly. The procedure checks to see if the two polys have the same variable. If so, div-poly strips off the variable and passes the problem to div-terms, which performs the division operation on term lists. Div-poly finally reattaches the variable to the result supplied by div-terms. It is convenient to design div-terms to compute both the quotient and the remainder of a division. Div-terms can take two term lists as arguments and return a list of the quotient term list and the remainder term list.

Complete the following definition of div-terms by filling in the missing expressions. Use this to implement div-poly, which takes two polys as arguments and returns a list of the quotient and remainder polys.

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     <compute rest of result recursively>
                     ))
                <form complete result>
                ))))))
-----------------------------------------------------------------------------------------------------------------
  (define (div-poly p1 p2)
    (define (same-var? seq)
      (reduce-right (lambda (x y)
		      (if (false? y)
			  false
			  (eq? x y)))
		  false
		  seq))
    (let ((variables (map variable (list p1 p2))))
      (if (not (same-var? variables))
	  (error "variables are not of the same type -- DIV-POLY " variables)
	  (let ((term-lists (map term-list (list p1 p2))))
	    (let ((div-result (apply div-terms term-lists)))
	      (list (make-poly (car variables) (choose-repr (car div-result)))
		    (make-poly (car variables) (choose-repr (cadr div-result)))))))))
  (define (div-terms L1 L2)
    (define (update-dividend L1 L2 new-term)
      (add-terms L1
		 (negate
		  (mul-term-by-all-terms new-term L2))))
    (if (empty-termlist? L1)
	(list (the-empty-termlist 'sparse) (the-empty-termlist 'sparse))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order T2) (order T1))
	      (list (the-empty-termlist 'sparse) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((new-term (make-term new-o new-c)))
		  (let ((rest-of-result (div-terms (update-dividend L1 L2 new-term)
						   L2)))
		    (list (add-terms (adjoin-term new-term (the-empty-termlist 'sparse))
				     (car rest-of-result))
			  (cadr rest-of-result)))))))))


(div (make-poly 'x '(sparse (20 1) (0 -1)))
     (make-poly 'x '(sparse (3 1) (2 1) (0 -1))))
;Value 66: ((polynomial x dense 1 -1 1 0 -1 2 -2 1 1 -3 4 -3 0 4 -7 7 -3 -4) (polynomial x dense 11 -3 -5))

