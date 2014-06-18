;; ALTERNATE DENSE-REPR IF NEEDED
;;  (define (dense-repr terms)
;;    (accumulate (lambda (x y)
;;		  (if (the-empty-termlist? y)
;;		      (cons (coeff x) (pad-zeros (order x) 0))
;;		      (append (cons (coeff x) (pad-zeros (order x) (order y-term))) y)))
;;		(the-empty-termlist)
;;		terms))



;; terms needed for each package:

;; FIRST-TERM, REST-TERMS, EMPTY-TERMLIST?, THE-EMPTY-TERMLIST, ADJOIN-TERM, MAX-ORDER
;; CARDINALITY, DENSE-REPR, SPARSE-REPR, NEGATE, POLY-ZER0

;; DECIDE WHETHER MAPS AND ACCUMULATES ARE A GOOD IDEA, OR WHETHER IT IS BETTER TO JUST USE
;; SELECTORS AND RECURSE (SEE POLY ZERO FOR EXAMPLE), THIS MIGHT BE POSSIBLE TO INSTALL
;; IN POLYNOMIAL PACKAGE, SEE IF THE SAME IS TRUE FOR NEGATE THROUGH SELECTORS

(define (first-term terms)
  (apply-generic 'first-term terms))
(define (rest-terms terms)
  (apply-generic 'rest-terms terms))
(define (empty-termlist? terms)
  (apply-generic 'empty-termlist? terms))
(define (the-empty-termlist type)
  ((get 'the-empty-termlist type)))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term terms)
  ((get 'adjoin-term (type-tag terms)) term (contents terms)))
(define (max-order terms)
  (apply-generic 'max-order terms))
(define (cardinality terms)
  (apply-generic 'cardinality terms))
(define (dense-repr terms)
  (apply-generic 'dense-repr terms))
(define (sparse-repr terms)
  (apply-generic 'sparse-repr terms))
(define (pad-zeros high low)
  (make-list (- high low) 0))

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))


(define (install-polynomial-package)

  ;;; REDEFINE THESE TERMS SO THAT THEY MAKE A CHOICE OF WHICH REPRESENTATION TO ADJOIN-TERM TO
  ;;; IN OTHER WORDS, DEFINE A PREDICATE TO DETERMINE WHICH REPRESENTATION IS BEST
  ;;; THIS COULD BE SOLVED BY ONLY MAKING RESULTS IN SPARSE FORM, BUT THEN CHECKING TO SEE
  ;;; IF THE POLYNOMIAL IS "DENSE" AFTER THE OPERATION HAS COMPLETED. OR VICE VERSA


  ;;; Need generic procedures max-order, num-terms, dense-repr, sparse-repr

  ;;; Need local poly-zero, neg

  (define (poly-zero? p)
    (define (zero-iter terms)
      (if (empty-termlist? terms)
	  true
	  (let ((first (first-term terms)))
	    (if (=zero? (coeff first))
		(zero-iter (rest-terms terms))
		false))))
    (zero-iter (term-list p)))

  (define (dense-poly? terms)
    (if (<= (max-order terms) (* (cardinality terms) 1.5))
	true
	false))
  (define (choose-repr terms)
    (if (dense-poly? terms)
	(dense-repr terms)
	(sparse-repr terms)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((added-terms (add-terms (term-list p1)
				      (term-list p2))))
	  (make-poly (variable p1) (choose-repr added-terms)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((multiplied-terms (mul-terms (term-list p1)
					   (term-list p2))))
	  (make-poly (variable p1) (choose-repr multiplied-terms)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist 'sparse)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist 'sparse)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (tag p) (attach-tag 'polynomial p))

  ;; interface to system

  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2)
				       (trace mul-poly)
				       (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negate (term-list p))))))
  (put 'zero '(polynomial) (lambda (p) (tag (poly-zero? p))))
  (put 'sparse-repr '(polynomial) (lambda (p) (sparse-repr (term-list p))))
  (put 'dense-repr '(polynomial) (lambda (p) (dense-repr (term-list p))))
  'done)
  
(define (install-sparse-package)
  ;; Operations on sparse term-lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (length-termlist terms) (length terms))
  (define (empty-termlist? term-list) (null? term-list))
  (define (sparse-repr terms)
    terms)

  (define (dense-repr terms)
    (define (first-term-order terms)
      (if (empty-termlist? terms)
	  0
	  (order (first-term terms))))
    (define (dense-iter terms previous-order)
      (let ((current-order (first-term-order terms))
	    (current-term (first-term terms)))
	(cond ((and (= (- previous-order 1) current-order) (= current-order 0))
	       (the-empty-termlist))
	      ((> (- previous-order 1) current-order)
	       (cons 0 (dense-iter terms (- previous-order 1))))
	      ((= (- previous-order 1) current-order)
	       (cons (coeff current-term) (dense-iter (rest-terms terms) (order current-term)))))))
	      

    (dense-iter terms (max-order terms)))

  
  (define (max-order terms) (order (first-term terms)))
  (define (cardinality terms) (length-termlist terms))
  (define (neg terms)
    (map (lambda (term) (negate (coeff term))) terms))
  (define (tag terms) (attach-tag 'sparse terms))
  ;;; Interface to outside

  (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(sparse) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(sparse) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(sparse) (lambda (terms)
				(trace sparse-repr)
				(tag (sparse-repr terms))))
  (put 'dense-repr '(sparse) (lambda (terms)
			       (trace dense-repr)
			       (attach-tag 'dense (dense-repr terms))))
  (put 'adjoin-term 'sparse (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(sparse) (lambda (terms) (cardinality terms)))
  (put 'max-order '(sparse) (lambda (terms) (max-order terms)))
  (put 'negate '(sparse) (lambda (p) (tag (neg p))))
  'done)
  

(define (install-dense-package)
  (define (zero-term? term) (= 0 (coeff term)))
  (define (max-order terms) (- (length terms) 1))
  (define (cardinality terms)
    (length (filter (lambda (x) (not (= x 0))) terms)))
  (define (the-empty-termlist) '())
  (define (empty-termlist? terms) (null? terms))
  (define (first-term terms)
    (make-term (max-order terms) (car terms)))
  (define (add-term t1 t2)
    (make-term (order t1) (+ (coeff t1) (coeff t2))))
  (define (rest-terms terms) (cdr terms))
  (define (dense-repr terms)
    terms)
  (define (order-index terms order)
    (- (max-order terms) order))
  (define (get-term-by-order terms order)
    (make-term order (list-ref terms (order-index terms order))))
  (define (adjoin-term term terms)
    (define (gen-append-list term terms max-order term-order)
      (if (> term-order max-order)
	  (extend-termlist term terms max-order term-order)
	  (insert-termlist term terms max-order term-order)))
    (define (extend-termlist t tl mo to)
      (append (list (coeff t))
	      (make-list (- to (+ mo 1)) 0)
	      tl))
    (define (insert-termlist t tl mo to)
      (append (list-head tl (- mo to))
	      (list (coeff (add-term (get-term-by-order tl to)
				     t)))
	      (list-tail tl (+ 1 (order-index tl to)))))
    (cond ((and (empty-termlist? terms) (= (order term) 0))
	   (list (coeff term)))
	  (else (gen-append-list term
				 terms
				 (max-order terms)
				 (order term)))))
  (define (sparse-repr terms)
    (cond ((empty-term-list? terms) (the-empty-termlist))
	  ((zero-term? (first-term terms)) terms)
	  (else (cons (first-term terms) (sparse-iter (rest-terms terms))))))
  (define (neg terms)
    (map (lambda (x) (negate x)) terms))
  (define (tag terms)
    (attach-tag 'dense terms))

  ;;; Interface to outside

  (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(dense) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(dense) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(dense) (lambda (terms) (tag (sparse-repr terms))))
  (put 'dense-repr '(dense) (lambda (terms) (tag (dense-repr terms))))
  (put 'adjoin-term 'dense (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(dense) (lambda (terms) (cardinality terms)))
  (put 'max-order '(dense) (lambda (terms) (max-order terms)))
  (put 'negate '(dense) (lambda (terms) (tag (neg terms))))
  'done)


  


(mul (make-poly 'x '(sparse (100 4)))
     (make-poly 'x '(sparse (5 3) (4 2) (1 1))))

(mul (make-poly 'x '(dense 5 0))
     (make-poly 'x '(dense 30)))

(sparse-repr (make-poly 'x '(dense 5 0)))



(define (install-packages)
  (install-scheme-number-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-polynomial-package)
  (install-dense-package)
  (install-sparse-package))

(install-packages)

(trace install-dense-package)

(define (test)
  (define (stuff)
    1)
  (trace stuff)
  (stuff))


(test)