###2.58###
PROMPT:
------------------------------------------------------------
Exercise 2.58.  Suppose we want to modify the differentiation program so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators. Since the differentiation program is defined in terms of abstract data, we can modify it to work with different representations of expressions solely by changing the predicates, selectors, and constructors that define the representation of the algebraic expressions on which the differentiator is to operate.

a. Show how to do this in order to differentiate algebraic expressions presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that + and * always take two arguments and that expressions are fully parenthesized.

b. The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?
------------------------------------------------------------
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product (make-product (exponent exp)
				     (make-exponentiation (base exp) (- (exponent exp) 1)))
		       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (sum? exp)
  (and (pair? exp) (eq? (cadr exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))

(define (addend exp)
  (car exp))
(define (augend exp)
  (caddr exp))

(define (multiplier exp)
  (car exp))
(define (multiplicand exp)
  (caddr exp))

(define (make-sum a b)
  (cond ((and (number? a) (number? b)) (+ a b))
	((=number? a 0) b)
	((=number? b 0) a)
	(else (list a '+ b))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
	((=number? a 1) b)
	((=number? b 1) a)
	((and (number? a) (number? b)) (* a b))
	(else (list a '* b))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
;Value: 4
(deriv '(x * (y * (x + 3))) 'x)
;Value 15: ((x * y) + (y * (x + 3)))



;;;;;;;PART B;;;;;;;

(define (operator exp)
  (cond ((memq '+ exp) '+)
	((memq '* exp) '*)))

(define (prefix op exp)
  (define (iter values remaining)
    (if (eq? op (car remaining))
	(reverse values)
	(iter (cons (car remaining) values)
	      (cdr remaining))))
  (iter '() exp))

(operator '(1 * 2 * 3 * 4))

(define (addend exp)
  (let ((p (prefix '+ exp)))
    (if (= (length p) 1)
	(car p)
	p)))

(define (augend exp)
  (let ((s (cdr (memq '+ exp))))
    (if (= (length s) 1)
	(car s)
	s)))

(define (sum? exp)
  (eq? (operator exp) '+))

(define (multiplier exp)
  (let ((p (prefix '* exp)))
    (if (= (length p) 1)
	(car p)
	p)))

(define (multiplicand exp)
  (let ((s (cdr (memq '* exp))))
    (if (= (length s) 1)
	(car s)
	s)))

(define (product? exp)
  (eq? (operator exp) '*))
	   

(deriv '(x + 3 * (x + y + 2)) 'x)
;Value: 4


;;; The idea for this code is borrowed from a comment on Bill the Lizard by someone named Alexey Grigorev. credit where credit is due...
;;; Once I read the first definition I realized what I had been missing. It is possible to make this work if the equation is split on
;;; addition first before multiplication. I got tripped up by the dead end of trying to make the it work by parsing each operator as
;;; the one element before and the rest being called as the rest of the list. Obviously this doesn't work when you have multiplication
;;; followed by any other elements in the list, becuase multiplication would have been applying the product rule to first and rest respectively
;;; as f(x) and g(x) and then halting.

