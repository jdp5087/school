###2.73###
PROMPT:
------------------------------------------------------------
Exercise 2.73.  Section 2.3.2 described a program that performs symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

We can regard this program as performing a dispatch on the type of the expression to be differentiated. In this situation the ``type tag'' of the datum is the algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform this program into data-directed style by rewriting the basic derivative procedure as

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

a.  Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?

b.  Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.

c.  Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and install it in this data-directed system.

d.  In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like

((get (operator exp) 'deriv) (operands exp) var)

What corresponding changes to the derivative system are required?
------------------------------------------------------------

;; (a) This procedure, just like the example of data-directed programming with complex numbers, assumes the existence of procedures put and get. The
;; the existence of put is implied because it would be necessary to construct a lookup table in order to use get to extract procedures.
;; The get procedure fetches a function tagged as an "operation" 'deriv, and a "type" which is the type of the expression, which could be
;; sum, product, exponent, etc. In this way, the deriv procedure has been transformed significantly, and no longer requires extra clauses to be
;; added every time a new form of differentiation needs to be added. The procedure just looks up 'deriv in the table according to the type
;; of differentiation needed. The get procedure will return either the correct procedure, or an error if the lookup fails to find the desired
;; procedure. 

;; The predicates number? and variable? cannot be included in the dispatch, because a variable or number is a single argument.
;; handing this procedure to get wouldn't work, because the operator procedure expects to be able to strip the first term off of a list. This would
;; result in an error.

;; (b and c)

(define (install-deriv-package)
  (define (sum-deriv operands var)
    (define (make-sum addend . rest)
      (let ((sum (accumulate (lambda (x y)
			       (cond ((=number? x 0) y)
				     ((and (not (null? y)) (number? x) (number? (car y)))
				      (cons (+ x (car y)) (cdr y)))
				     (else (cons x y))))
			     '()
			     (append (list addend) rest))))
	(if (= (length sum) 1)
	    (car sum)
	    (cons '+ sum))))
    (define (addend s) (car s))
    (define (augend s)
      (if (null? (cddr s))
	  (cadr s)
	  (apply make-sum (cdr s))))
    (make-sum (deriv (addend operands) var)
	      (deriv (augend operands) var)))
  (define (prod-deriv operands exp)
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
				 ((and (not (null? y)) (number? x) (number? (car y)))
				  (cons (* x (car y)) (cdr y)))
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
	  (apply make-product (cddr p)))))
    (make-sum (make-product (multiplier operands)
			    (deriv (multiplicand operands) exp))
	      (make-product (deriv (multiplier operands) exp)
			    (multiplicand operands)))
    (define (exp-deriv operands var)
      (define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**)))
      (define (base x)
	(cadr x))
      (define (exponent x)
	(caddr x))
      (define (make-exponentiation base exp)
	(cond ((=number? exp 0) 1)
	      ((=number? exp 1) base)
	      (else (list '** base exp))))
      (make-exponentiation (make-product (exponent operands)
					 (make-exponentiation (base operands) (- (exponent operands) 1))
					 (deriv (base operands)))))
			
    (put 'deriv '+ sum-deriv)
    (put 'deriv '* prod-deriv)
    (put 'deriv '** exp-deriv))

(d) 

(put '+ 'deriv sum-deriv)
(put '* 'deriv prod-deriv)
(put '** 'deriv exp-deriv)

These simple changes should do it. 
		    


