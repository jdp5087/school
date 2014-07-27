;; scheme basics

;; leftmost expression or primitive (operator) is applied to the rest of the list (operands)

1
;Value: 1

+
;Value 172: #[arity-dispatched-procedure 172]

(+ 1 1)
;Value: 2

(if true
    1
    2)
;Value: 1

(define (my-func x)
  (cond ((= x 0) "zero!")
	((= x 1) "one")
	((> x 1) "greater than 1")
	(else "less than zero")))
(my-func 0)
;Value 174: "zero!"

(define (my-higher-order-function sequence function)
  (map function sequence))

(my-higher-order-function (list 0 2 -1) my-func)
;Value 179: ("zero!" "greater than 1" "less than zero")

(fold-right (lambda (x y)
	      (if (= x 2)
		  (+ x y)
		  y))
	    0
	    (list 2 1 2 1 2))
;Value: 6

(cons 1 2)
;Value 182: (1 . 2)

(cons 2 (cons 1 '()))
;Value 183: (2 1)

(fold-right cons '() (list 4 3 2 1))
;Value 185: (4 3 2 1)

;; Arithmetic generic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ x y))
(define (=zero? x) (apply-generic 'zero x))
(define (negate x) (apply-generic 'negate x))
(define (rationalize-approx real-num)
  (apply-generic 'rationalize-approx real-num 100))
(define (numer x)
  (if (equal? (type-tag x) 'rational)
      (apply-generic 'numer x)
      (error "object is not of type rational -- NUMER -- " x)))
(define (denom x)
  (if (equal? (type-tag x) 'rational)
      (apply-generic 'denom x)
      (error "object is not of type rational -- DENOM  -- " x)))

;; Trigonometric generic procedures
(define (sin-generic x) (apply-generic 'sin x))
(define (cos-generic x) (apply-generic 'cos x))
(define (atan-generic x y) (apply-generic 'atan x y))
(define (square-generic x) (apply-generic 'square x))
(define (sqrt-generic x) (apply-generic 'sqrt x))

;; Imaginary number generic procedures
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Straightforward set implementation (not efficient).
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define empty-set '())

;; determine if an object is a rational function (rational polynomial)
(define (rat-function? obj)
  (if (not (equal? (type-tag obj) 'rational))
      false
      (let ((n (numer obj))
	    (d (denom obj)))
	(if (not (or (poly? n) (poly? d)))
	    false
	    true))))

;; Tagging procedures
(define (attach-tag type-tag contents)
  (cond ((and (number? contents)
	      (not (string=? (number->string contents) (number->string (inexact->exact contents)))))
	 (cons type-tag contents))
	((number? contents) contents)
	(else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((null? datum) 'empty)
	((boolean? datum) 'boolean)
	((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))

;; hash table definition, along with get and put procedures
(define *op-table* (make-hash-table equal?))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) false))

;; hash tables for coercion procedures
(define *coercion-types* (make-equal-hash-table))
(define *coercion-table* (make-equal-hash-table))

;; get and put functions to see a set of all types that can be converted TO
;; stored in a set
(define (add-type-to-coercion-types type)
  (hash-table/put! *coercion-types*
		   'all-types
		   (adjoin-set type (get-all-coercion-types))))
(define (get-all-coercion-types)
  (hash-table/get *coercion-types* 'all-types empty-set))

;; Tracks the conversions that can be made to any type. For example, if two conversions exist to 'rational,
;; then (get-coercions-to 'rational) will return two entries. Stored in a simple unordered set
;; (just to hack a solution together. otherwise I'd spend more time optimising).
(define (get-coercions-to to-type)
  (hash-table/get *coercion-types* to-type empty-set))
(define (put-coercions-to to-type from-type)
  (hash-table/put! *coercion-types* to-type (adjoin-set from-type (get-coercions-to to-type))))

;; The same as in the readings, procedures to access a table that stores coercions,
;; However, every time a coercion procedure is added, these procedures track the
;; types of procedures that can be applied.
(define (put-coercion from-type to-type proc)
  (add-type-to-coercion-types to-type)
  (put-coercions-to to-type from-type)
  (hash-table/put! *coercion-table* (list from-type to-type) proc))
(define (get-coercion-procedure from-type to-type)
  (hash-table/get *coercion-table* (list from-type to-type) false))

;; This will iterate each type that can be coerced to.
;; for that type, a set of all the types that can be coerced to that type will be accessed
;; If all of the needed-types are in that set, a coercion is returned, otherwise
;; the next available to-type is checked, and if all are exausted, false is returned.
(define (get-coercion needed-types)
  (define (type-in-list? t l)
    (cond ((null? l) false)
	  ((eq? t (car l)) true)
	  (else (type-in-list? t (cdr l)))))
  (define (viable? type needed)
    (cond ((null? needed) true)
	  ((equal? (car needed) type) (viable? type (cdr needed)))
	  ((element-of-set? (car needed) (get-coercions-to type)) (viable? type (cdr needed)))
	  (else false)))
    (define (iter-types types)
    (cond ((null? types) false)
	  ((viable? (car types) needed-types) (car types))
	  (else (iter-types (cdr types)))))
  (let ((all-coercion-types (get-all-coercion-types)))
    (iter-types all-coercion-types)))

;; returns a lambda function that coerces any given type to a set type.
;; This will only be called if all types can be coerced to the desired type.
(define (construct-apply-coercion to-type)
  (lambda (from-object)
    (let ((from-type (type-tag from-object)))
      (if (equal? from-type to-type)
	  from-object
	  ((get-coercion-procedure from-type to-type) from-object)))))

;; Integer package
;; Note: scheme-numbers are not tagged, see type-tag procedure,
;; which checks if the element is an integer, if it is, type tag calls
;; it a scheme-number, otherwise, it checks for a type tag
(define (install-scheme-number-package)
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put 'zero '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'sin '(scheme-number) (lambda (x) (sin x)))
  (put 'cos '(scheme-number) (lambda (x) (cos x)))
  (put 'atan '(scheme-number scheme-number) (lambda (x y) (atan x y)))
  (put 'square '(scheme-number) (lambda (x) (square x)))
  (put 'sqrt '(scheme-number) (lambda (x) (sqrt x)))
  (put 'negate '(scheme-number) (lambda (x) (* -1 x)))
  (put 'gcd '(scheme-number scheme-number) gcd)
  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  (put-coercion 'scheme-number
		'rational
		(lambda (x) (make-rational x 1)))
  'done)



;; rational package
;; All operations on rationals are themselves expressed as
;; generic procedures, which allows a rational to store polynomials,
;; imaginaries, and other complex types (so long as these types have defined
;; all of the required generic procedures.
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equal x y) (and (equ? (numer x) (numer y))
			   (equ? (denom x) (denom y))))
  (define (make-rat n d)
    (let ((r (reduce n d)))
      (cons (car r) (cadr r))))
  (define (math-generic-rat proc . args)
    (let ((real-args (map (lambda (x) (raise (tag x))) args)))
      (let ((result (apply apply-generic (cons proc real-args))))
	(let ((dropped-result (drop result)))
	  (if (eq? 'real (type-tag dropped-result))
	      (rationalize-approx result)
	      dropped-result)))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ '(rational rational) equal)
  (put 'zero '(rational) (lambda (x) (equ? (numer x) 0)))
  (put 'cos '(rational) (lambda (x) (math-generic-rat 'cos x)))
  (put 'sin '(rational) (lambda (x) (math-generic-rat 'sin x)))
  (put 'atan '(rational rational) (lambda (x y) (math-generic-rat 'atan x y)))
  (put 'square '(rational) (lambda (x) (math-generic-rat 'square x)))
  (put 'sqrt '(rational) (lambda (x) (math-generic-rat 'sqrt x)))
  (put 'negate '(rational) (lambda (x) (tag (mul-rat (make-rat -1 1) x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put-coercion 'rational
		'real
		(lambda (x)
		  (let ((num-part (contents x)))
		    (make-real (/ (numer num-part) (denom num-part))))))
  (put-coercion 'rational
		'scheme-number
		(lambda (x)
		  (let ((rational-number (contents x)))
		    (numer rational-number))))
		  
  'done)

;; Create a rational number
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Real number package
(define (install-real-package)
  (define (rationalize real-num)
    (define (rationalize-iter num den)
      (if (= (truncate num) num)
	  (make-rational num den)
	  (rationalize-iter (* num 10) (* den 10))))
    (rationalize-iter real-num 1))
  (define (rationalize-approx number tolerance-denom)
    (define (find-sign number)
      (if (>= number 0)
	  1
	  -1))
    (define (rationalize-iter num den sign)
      (if (<= (abs (- (abs number) (/ num den))) (/ 1 tolerance-denom))
	  (make-rational (* num sign) den)
	  (rationalize-iter (+ num 1) den sign)))
    (let ((sign (find-sign number))
	  (abs-num (abs (truncate number))))
      (rationalize-iter (* abs-num tolerance-denom) tolerance-denom sign)))

  (define (tag z)
    (attach-tag 'real z))
  (define (add-real x y)
    (+ x y))
  (define (sub-real x y)
    (- x y))
  (define (mul-real x y)
    (* x y))
  (define (div-real x y)
    (/ x y))
  (define (make-real x)
    (if (number? x)
	(* x 1.0)
	(error "Not a number -- MAKE-REAL " x)))
  (put 'add '(real real) (lambda (x y) (tag (add-real x y))))
  (put 'sub '(real real) (lambda (x y) (tag (sub-real x y))))
  (put 'mul '(real real) (lambda (x y) (tag (mul-real x y))))
  (put 'div '(real real) (lambda (x y) (tag (div-real x y))))
  (put 'equ '(real real) (lambda (x y) (= x y)))
  (put 'zero '(real real) (lambda (x) (= x 0)))
  (put 'sin '(real) (lambda (x) (tag (sin x))))
  (put 'cos '(real) (lambda (x) (tag (cos x))))
  (put 'atan '(real real) (lambda (x y) (tag (atan x y))))
  (put 'square '(real) (lambda (x) (tag (square x))))
  (put 'sqrt '(real) (lambda (x) (tag (square x))))
  (put 'negate '(real) (lambda (x) (tag (* -1 x))))
  (put 'make 'real (lambda (x) (tag (make-real x))))
  (put 'rationalize-approx '(real scheme-number) (lambda (number tolerance-denom)
						   (rationalize-approx number tolerance-denom)))
			      
  (put-coercion 'real
		'complex
		(lambda (x)
		  (let ((num-part (contents x)))
		    (make-complex-from-real-imag num-part 0))))
  (put-coercion 'real
		'rational
		(lambda (x)
		  (let ((num (contents x)))
		    (rationalize num)))))
;; Make a real number
(define (make-real x)
  ((get 'make 'real) x))
	   
;; Complex number package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex) equ?)
  (put 'zero '(complex) =zero?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'negate '(complex)
       (lambda (x) (tag (make-from-real-imag (negate (real-part x))
					     (negate (imag-part x))))))
  (put-coercion 'complex
		'real
		(lambda (x)
		  (define (raise-to-real obj)
		    (if (eq? (type-tag obj) 'real)
			obj
			(raise-to-real (raise obj))))
		  (raise-to-real (real-part x))))
  'done)

;; Make a complex number
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Rectangular representation for complex numbers
;; (x-axis is real, y-axis is imaginary)
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (equal x y) (and (equ? (real-part x) (real-part y))
			   (equ? (imag-part x) (imag-part y))))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-generic (add (square-generic (real-part z))
		       (square-generic (imag-part z)))))
  (define (angle z)
    (atan-generic (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cos-generic a)) (mul r (sin-generic a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(rectangular rectangular) equal)
  (put 'zero '(rectangular) (lambda (x) (=zero? (magnitude x))))
  'done)

;; Magnitude-angle representation
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (equal x y) (and (equ? (magnitude x) (magnitude y))
			   (equ? (angle x) (angle y))))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos-generic (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin-generic (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt-generic (add (square-generic x) (square-generic y)))
          (atan-generic y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(polar polar) equal)
  (put 'zero '(polar) (lambda (x) (equ? (magnitude x) 0)))
  'done)

;; procedures to manipulate number hierarchies
(define (find-one-up type)
  (let ((result (memq type (get-tower-hierarchy))))
    (if (> (length result) 1)
	(cadr result)
	(error "Error - type not raisable -- FIND-ONE-UP " type))))
(define (find-one-down type)
  (let ((result (memq type (reverse (get-tower-hierarchy)))))
    (if (> (length result) 1)
	(cadr result)
	(error "Error -- type not droppable -- FIND-ONE-DOWN " type))))
(define (put-tower-hierarchy hierarchy)
  (hash-table/put! *coercion-table* 'hierarchy hierarchy))
(define (get-tower-hierarchy)
  (hash-table/get *coercion-table* 'hierarchy '()))
(put-tower-hierarchy '(scheme-number rational real complex))

;; force an object up one type in the hierarchy
(define (raise obj)
  (let ((obj-type (type-tag obj)))
    ((get-coercion-procedure obj-type (find-one-up obj-type)) obj)))
;; force an object down one type in hierarchy
(define (project obj)
  (let ((obj-tag (type-tag obj)))
    (let ((reverse-hierarchy (reverse (get-tower-hierarchy))))
      (let ((projection (get-coercion-procedure obj-tag (find-one-down obj-tag))))
	(if projection
	    (projection obj)
	    (else "error -- projection not found for this procedure -- PROJECT -- " obj))))))
;; force an object down, raise it back up, if the result is equal to original
;; object, project down again.
(define (drop obj)
  (if (equal? (raise (project obj)) obj)
      (project obj)
      obj))

;; New definition of apply-generic
(define (apply-generic op . args)
  ;; drop an object down to lowest available type in hierarchy
  ;; when object is projected and raised but the object is not the same,
  ;; the object won't be dropped
  (define (apply-drop obj)
    (cond ((rat-function? obj) obj)
	  ((equal? (type-tag obj) (car (get-tower-hierarchy))) obj)
	  (else (let ((drop-result (drop obj)))
		  (if (equal? obj drop-result)
		      obj
		      (apply-drop drop-result))))))
  ;; find the highest type in a list of type tags
  (define (find-highest-type types)
    (define (highest-iter types-remaining highest)
      (if (null? types-remaining)
	  (car highest)
	  (let ((current-rank (memq (car types-remaining) (get-tower-hierarchy))))
	    (cond ((< (length current-rank) 1) (highest-iter (cdr types-remaining) highest))
		  ((< (length current-rank) (length highest)) (highest-iter (cdr types-remaining) current-rank))
		  (else (highest-iter (cdr types-remaining) highest))))))
    (highest-iter types (get-tower-hierarchy)))
  (define (lower-rank? a b)
    (let ((rank-a (length (memq a (get-tower-hierarchy))))
	  (rank-b (length (memq b (get-tower-hierarchy)))))
      (if (> rank-a rank-b)
	  true
	  false)))
  (define (raise-to-rank obj target)
    (if (not (lower-rank? (type-tag obj) target))
	obj
	(raise-to-rank (raise obj) target)))
  (define (all-types-equivalent? types)
    (cond ((null? (cdr types)) true)
	  ((equal? (car types) (cadr types)) (all-types-equivalent? (cdr types)))
	  (else false)))
  ;; get a list of all type tags in operands of arbitrary length
  (let ((type-tags (map type-tag args)))
    ;; attempt to get a procedure from lookup table with current type tags
    (let ((proc (get op type-tags)))
      (if proc
	  ;; if found, apply result and drop if resulting type tag is part of hierarchy
          (let ((apply-result (apply proc (map contents args))))
	    (if (not (member (type-tag apply-result) (get-tower-hierarchy)))
		apply-result
		(apply-drop apply-result)))
	  (if (all-types-equivalent? type-tags) ;; If all types are the same then a procedure doesn't exist
	      (error "No method found: all types are equivalent -- APPLY-GENERIC" (list op type-tags))
	      (let ((highest (find-highest-type type-tags))) ;; find the highest rank in type hierarchy in the current arguments
		(apply apply-generic (cons op (map (lambda (x)
						     (raise-to-rank x highest))
						   args))))))))) ;; raise every object to the highest rank and call apply generic


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Polynomial Packages;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generic procedures for polynomials (independent of underlying representation)
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
(define (find-vars poly)
  (apply-generic 'find-vars poly))
(define (poly? x)
  (and (pair? x) (eq? (type-tag x) 'polynomial)))
(define (remove-duplicate-vars vars)
  (define (dup-iter seq result)
    (cond ((null? seq) result)
	  ((member (car seq) result) (dup-iter (cdr seq) result))
	  (else (dup-iter (cdr seq) (append result (list (car seq)))))))
  (dup-iter vars '()))
(define (order-vars vars)
  (sort vars (lambda (x y)
	       (let ((x-length (length (memq x (variable-hierarchy))))
		     (y-length (length (memq y (variable-hierarchy)))))
		 (> x-length y-length)))))
(define (get-ordered-variables poly)
  (order-vars (remove-duplicate-vars (find-vars poly))))
(define (expand-poly p)
  (if (poly? p)
      (apply-generic 'expand p)
      p))

;; Not being used currently
(define (compress-polynomial p)
  (apply-generic 'compress-polynomial p))

;; provides an order of variables for cannonical form
(define (variable-hierarchy)
  (list 'w 'z 'y 'x))

;; make a polynomial
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Creates one termlist from two but does not add terms
(define (adjoin-termlists L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	 (let ((t1 (first-term L1))
	       (t2 (first-term L2)))
	   (cond ((> (order t1) (order t2))
		  (adjoin-term t1
			       (adjoin-termlists (rest-terms L1)
						 L2)))
		 ((< (order t1) (order t2))
		  (adjoin-term t2
			       (adjoin-termlists L1
						 (rest-terms L2))))
		 (else (adjoin-term t1
				    (adjoin-term t2
						 (adjoin-termlists (rest-terms L1)
								   (rest-terms L2))))))))))

;; generic functions to reduce a polynomial to least terms
(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))
(define (reduce n d)
  (apply-generic 'reduce n d))

;; polynomial package
;; A polynomial can have one of two underlying representations
;; Dense, or Sparse, which are installed in seperate packages
(define (install-polynomial-package)
  ;; 
  (define (expand p)
    (let ((terms (term-list p)))
      (let ((result (expand-termlist terms)))
	(make-poly (variable p)
		   (expand-result result)))))
  (define (expand-termlist terms)
    (if (empty-termlist? terms)
	(the-empty-termlist 'sparse)
	(let ((first (first-term terms)))
	  (if (poly? (coeff first))
	      (adjoin-term (make-term (order first) (expand-poly (coeff first)))
			   (expand-termlist (rest-terms terms)))
	      (adjoin-term first
			   (expand-termlist (rest-terms terms)))))))
  (define (expand-result terms)
    (if (empty-termlist? terms)
	(the-empty-termlist (type-tag terms))
	(adjoin-termlists (expand-term (first-term terms))
			  (expand-result (rest-terms terms)))))

  (define (expand-term term)
    (define (expand-term-iter lower-terms built-terms lower-var)
      (if (empty-termlist? lower-terms)
	  built-terms
	  (let ((first-lower (first-term lower-terms)))
	    (expand-term-iter (rest-terms lower-terms)
			      (adjoin-term (make-term (order term)
						      (tag (make-poly lower-var
								      (adjoin-term first-lower
										   (the-empty-termlist 'sparse)))))
					   built-terms)
					   lower-var))))
    (if (not (poly? (coeff term)))
	(adjoin-term term (the-empty-termlist 'sparse))
	(let ((poly-contents (contents (coeff term))))
	  (let ((lower-var (variable poly-contents))
		(lower-terms (term-list poly-contents)))
	    (expand-term-iter lower-terms (the-empty-termlist 'sparse) lower-var)))))

  ;; functions to compress a polynomial after add, sub, or mul (simplification)
  (define (compress-poly p)
      (make-poly (variable p) (compress-termlist (term-list p))))
  (define (compress-termlist terms)
      (compress-terms (compress-coeffs terms)))
  (define (compress-coeffs terms)
    (if (empty-termlist? terms)
	(the-empty-termlist 'dense)
	(let ((first (first-term terms)))
	  (if (poly? (coeff first))
	      (adjoin-term (make-term (order first)
				      (compress-polynomial (coeff first)))
			   (compress-coeffs (rest-terms terms)))
	      (adjoin-term first (compress-coeffs (rest-terms terms)))))))
  (define (compress-terms terms)
    (define (compress-iter first rest)
      (if (empty-termlist? terms)
	  (the-empty-termlist 'sparse)
	  (let ((result (compress-term first rest)))
	    (if (empty-termlist? (cdr result))
		(adjoin-term (car result) (cdr result))
		(adjoin-term (car result)
			     (compress-iter (first-term (cdr result))
					    (rest-terms (cdr result))))))))
    (compress-iter (first-term terms) (rest-terms terms)))
  (define (compress-term first rest)
    (define (compress-term-iter first rest viable)
      (if (empty-termlist? rest)
	  (cons first viable)
	  (let ((next (first-term rest)))
	    (if (= (order first) (order next))
		(compress-term-iter (make-term (order first)
					       (add (coeff first) ;;; IF SAME TYPE AND NOT POLY ADD, IF SAME TYPE AND POLY AND IN SAME VARS ADD, ELSE
						    (coeff next))) ;;; DON'T DO ANYTHING, ALSO MAKE SURE THAT COMPRESS IS ACTUALLY WORKING RIGHT
				    (rest-terms rest)
				    viable)
		(compress-term-iter first
				    (rest-terms rest)
				    (adjoin-term next viable))))))
    (compress-term-iter first rest (the-empty-termlist 'sparse)))
	  
  ;; create seperate polynomials for each term, and store all in a list
  ;; This step greatly simplifies the next step, becuase semantics will be
  ;; identical after re-ordering when we are only dealing with one term per
  ;; coefficient. For example, 4xyz is the same as zyx4.

  (define (seperate-expanded-poly p)
    (let ((top-var (variable p))
	  (top-terms (term-list p)))
      (define (sep-iter terms)
	(if (empty-termlist? terms)
	    '()
	    (cons (make-polynomial top-var
				   (adjoin-term (first-term terms)
						(the-empty-termlist 'sparse)))
		  (sep-iter (rest-terms terms)))))
      (sep-iter top-terms)))

  ;; adds extra terms so every polynomial term
  ;; has identical structure. For instance, if the union of
  ;; all variables in an operation are xyz, but a term has only
  ;; x, then we need to add y^0 and z^0 so that structure is identical
  (define (pad-seperated-poly sp vars)
    (if (null? sp)
	'()
	(cons (pad-poly (car sp) vars)
	      (pad-seperated-poly (cdr sp) vars))))
  (define (pad-poly p vars)
    (let ((poly-vars (find-vars p)))
      (let ((needed-vars (remove (lambda (x)
				   (member x poly-vars))
				 vars)))
	(if (null? needed-vars)
	    p
	    (pad-poly (make-polynomial (car needed-vars)
				       (adjoin-term (make-term 0 p)
						    (the-empty-termlist 'sparse)))
		      vars)))))

  ;; procedures to put terms in order (based on order in variable hierarchy)
  (define (in-order? v1 v2)
    (let ((v1-rank (length (memq v1 (variable-hierarchy))))
	  (v2-rank (length (memq v2 (variable-hierarchy)))))
      (< v1-rank v2-rank)))
  (define (order-seperated-poly sp vars)
    (if (null? sp)
	'()
	(cons (order-and-check (car sp) vars)
	      (order-seperated-poly (cdr sp) vars))))
  (define (order-and-check p vars)
    (define (same-order? vars1 vars2)
      (cond ((and (null? vars1) (null? vars2)) true)
	    ((null? vars1) false)
	    ((null? vars2) false)
	    ((eq? (car vars1) (car vars2)) (same-order? (cdr vars1) (cdr vars2)))
	    (else false)))
    (if (same-order? (reverse (find-vars p)) vars)
	p
	(begin
	  (order-and-check (order-poly p vars) vars))))
  (define (order-poly p vars)
    (let ((top-contents (contents p)))
      (let ((high-var (variable top-contents))
	    (high-termlist (term-list top-contents)))
	(let ((high-order (order (first-term high-termlist)))
	      (high-coeff (coeff (first-term high-termlist))))
	  (if (not (poly? high-coeff))
	      p
	      (let ((low-contents (contents high-coeff)))
		(let ((low-var (variable low-contents))
		      (low-termlist (term-list low-contents)))
		  (if (in-order? high-var low-var)
		      (make-polynomial high-var
				       (adjoin-term (make-term high-order
							       (order-poly high-coeff vars))
						    (the-empty-termlist 'sparse)))
		      (let ((low-order (order (first-term low-termlist)))
			    (low-coeff (coeff (first-term low-termlist))))
			(make-polynomial low-var
					 (adjoin-term (make-term low-order
								 (order-poly (make-polynomial high-var
											      (adjoin-term (make-term high-order
														      low-coeff)
													   (the-empty-termlist 'sparse)))
									     vars))
						      (the-empty-termlist 'sparse))))))))))))

  ;; Join a seperated polynomial back together
  (define (get-first-term-from-poly p)
    (first-term (term-list (contents p))))
  (define (join-seperated-poly sp)
    (make-polynomial (variable (contents (car sp)))
		     (create-termlist-from-sep sp)))
  (define (create-termlist-from-sep sp)
    (if (null? sp)
	(the-empty-termlist 'sparse)
	(adjoin-term (get-first-term-from-poly (car sp))
		     (create-termlist-from-sep (cdr sp)))))
  (define (get-ordered-variables-inner poly)
    (order-vars (remove-duplicate-vars (find-v poly))))
  (define (get-longer-var-list vars1 vars2)
    (let ((len1 (length vars1))
	  (len2 (length vars2)))
      (if (> len1 len2)
	  vars1
	  vars2)))

  ;; Overall function, which expands, seperates, pads, orders, and rejoins a polynomial
  (define (canonical-form p vars)
    (join-seperated-poly (order-seperated-poly (pad-seperated-poly (seperate-expanded-poly (expand p)) vars) vars)))
  (define (get-canonical-forms p1 p2)
    (let ((vars1 (get-ordered-variables-inner p1))
	  (vars2 (get-ordered-variables-inner p2)))
      (let ((varlist (get-longer-var-list vars1 vars2)))
	(cons (contents (canonical-form p1 varlist))
	      (contents (canonical-form p2 varlist))))))
  (define (find-v p)
    (append (list (variable p)) (find-vars (term-list p))))

  (define (poly-zero? p)
    (define (zero-iter terms)
      (if (empty-termlist? terms)
	  true
	  (let ((first (first-term terms)))
	    (if (=zero? (coeff first))
		(zero-iter (rest-terms terms))
		false))))
    (zero-iter (term-list p)))
  (define (poly-equ? p1 p2)
    (if (not (eq? (variable p1) (variable p2)))
	false
	(equal-termlists? (term-list p1) (term-list p2))))
  (define (equal-termlists? terms1 terms2)
    (cond ((and (empty-termlist? terms1) (empty-termlist? terms2))
	   true)
	  ((empty-termlist? terms1) false)
	  ((empty-termlist? terms2) false)
	  (else (let ((ft1 (first-term terms1))
		      (ft2 (first-terms terms2)))
		  (if (not (and (= (order ft1) (order ft2))
				(equ? (coeff ft1) (coeff ft2))))
		      false
		      (equal-termlists? (rest-terms terms1) (rest-terms terms2)))))))

  ;; Determine if polynomial is expanded
  (define (expanded-poly? terms)
    (define (expanded-iter terms orders)
      (if (empty-termlist? terms)
	  false
	  (let ((first (first-term terms)))
	    (if (member (order first) orders)
		true
		(expanded-iter (rest-terms terms) (cons (order first) orders))))))
    (expanded-iter terms '()))

  ;; Compress polynomial if it is expanded (not being used)
  (define (compress-poly-if-needed terms)
    (if (expanded-poly? terms)
	(compress-termlist terms)
	terms))

  ;; procedures to choose proper representation for polynomials (disabled)
  (define (dense-poly? terms)
    (if (<= (max-order terms) (* (cardinality terms) 1.5))
	true
	false))
  (define (choose-repr terms)
    (let ((terms (compress-poly-if-needed terms)))
      (if (dense-poly? terms)
	  (dense-repr terms)
	  (sparse-repr terms))))

  ;; algebraic manipulation of polynomials
  (define (add-poly p1 p2)
    (let ((can-forms (get-canonical-forms p1 p2)))
      (let ((p1 (car can-forms))
	    (p2 (cdr can-forms)))
	(if (same-variable? (variable p1) (variable p2))
	      (let ((added-terms (add-terms (term-list p1)
					    (term-list p2))))
		(make-poly (variable p1) added-terms)) ;;; REMOVED CHOOSE REPR HERE
	      (error "Polys not in same var -- ADD-POLY"
		     (list p1 p2))))))

  (define (mul-poly p1 p2)
    (let ((can-forms (get-canonical-forms p1 p2)))
      (let ((p1 (car can-forms))
	    (p2 (cdr can-forms)))
	(if (same-variable? (variable p1) (variable p2))
	    (let ((multiplied-terms (mul-terms (term-list p1)
					       (term-list p2))))
	      (make-poly (variable p1) multiplied-terms)) ;;; REMOVED CHOOSE REPR HERE
	    (error "Polys not in same var -- MUL-POLY"
		   (list p1 p2))))))
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
	      (list (make-poly (car variables) (car div-result)) ;;; REMOVED CHOOSE REPR
		    (make-poly (car variables) (cadr div-result)))))))) ;;; REMOVED CHOOSE REPR
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

  ;; Procedures for reducing a rational polynomial to lowest terms
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (pseudoremainder-terms a b)
    (let ((order-a (order (first-term a)))
	  (order-b (order (first-term b)))
	  (coeff-b (coeff (first-term b))))
      (let ((integerizing-factor (expt coeff-b (- (+ 1 order-a) order-b))))
	(cadr (div-terms (mul-term-by-all-terms (make-term 0 integerizing-factor)
						a)
			 b)))))
  (define (extract-coeffs terms)
    (if (empty-termlist? terms)
	'()
	(cons (coeff (first-term terms))
	      (extract-coeffs (rest-terms terms)))))
  (define (find-total-gcd a)
    (let ((last-term (car a)))
      (let ((all-terms (append (list last-term) (reverse a))))
	(reduce-right gcd false all-terms))))
  (define (div-coeffs-by-constant terms constant)
    (car (div-terms terms
		    (adjoin-term (make-term 0 constant)
				 (the-empty-termlist 'sparse)))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
	(div-coeffs-by-constant a (find-total-gcd (extract-coeffs a)))
	(gcd-terms b (pseudoremainder-terms a b))))
  (define (gcd-poly p1 p2)
    (if (not (equal? (variable p1) (variable p2)))
	(error "polys not in same var -- GCD-POLY -- " (list (variable p1) (variable p2)))
	(make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))))
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (let ((c (coeff (first-term g)))
	    (o2 (order (first-term g)))
	    (n-o (order (first-term n)))
	    (d-o (order (first-term d))))
	(let ((o1 (if (> n-o d-o) n-o d-o)))
	  (let ((int-coeff (expt c (- (+ 1 o1) o2))))
	    (let ((int-term (make-term 0 int-coeff)))
	      (let ((reduced-n (car (div-terms (mul-term-by-all-terms int-term n) g)))
		    (reduced-d (car (div-terms (mul-term-by-all-terms int-term d) g))))
		(let ((gcd-coeffs (find-total-gcd (append (extract-coeffs reduced-n)
							  (extract-coeffs reduced-d)))))
		  (list (car (div-terms reduced-n (adjoin-term (make-term 0 gcd-coeffs)
							       (the-empty-termlist 'sparse))))
			(car (div-terms reduced-d (adjoin-term (make-term 0 gcd-coeffs)
							       (the-empty-termlist 'sparse)))))))))))))
  (define (reduce-poly n d)
    (let ((v1 (variable n))
	  (v2 (variable d)))
      (if (not (eq? v1 v2))
	  (error "numerator and denominator not in the same var -- REDUCE-POLY -- " (list n d))
	  (let ((reduced-termlists (reduce-terms (term-list n) (term-list d))))
	    (list (make-poly v1 (car reduced-termlists))
		  (make-poly v2 (cadr reduced-termlists)))))))
  
  ;; Selectors for polynomials
  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (tag p) (attach-tag 'polynomial p))

  ;; interface to system

  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (add-poly p1
			(make-poly (variable p2)
				   (negate (term-list p2)))))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2)
				       (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2)
				       (let ((result (div-poly p1 p2)))
					 (list (tag (car result)) (tag (cadr result))))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negate (term-list p))))))
  (put 'zero '(polynomial) (lambda (p) (poly-zero? p)))
  (put 'equ '(polynomial polynomial) poly-equ?)
  (put 'sparse-repr '(polynomial) (lambda (p) (sparse-repr (term-list p))))
  (put 'dense-repr '(polynomial) (lambda (p) (dense-repr (term-list p))))
  (put 'find-vars '(polynomial) (lambda (p) (find-v p)))
  (put 'expand '(polynomial) (lambda (p) (tag (expand p))))
  (put 'compress-polynomial '(polynomial) (lambda (p) (tag (compress-poly p))))
  (put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (n d)
	 (let ((r (reduce-poly n d)))
	   (list (tag (car r)) (tag (cadr r))))))
  'done)

;; Sparse polynomial representation
;; Each term stores order and coefficient
(define (install-sparse-package)
  ;; Operations on sparse term-lists

  (define (find-v terms)
      (if (empty-termlist? terms)
	  (the-empty-termlist)
	  (let ((first (first-term terms)))	  
	    (cond ((poly? (coeff first)) (append (find-vars (coeff first)) (find-v (rest-terms terms))))
		  (else (find-v (rest-terms terms)))))))	  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  ;; selectors for sparse polynomials
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (length-termlist terms) (length terms))
  (define (empty-termlist? term-list) (null? term-list))
  (define (sparse-repr terms)
    terms)

  ;; produce a dense representation from a sparse representation
  (define (dense-repr terms)
    (define (dense-iter terms prev-order)
      (cond ((and (empty-termlist? terms) (= prev-order 0))
	     (the-empty-termlist))
	    ((and (empty-termlist? terms) (> prev-order 0))
	     (cons 0 (dense-iter terms (- prev-order 1))))
	    (else
	     (let ((current-term (first-term terms))
		   (current-order (order (first-term terms))))
	       (cond ((= (- prev-order 1) current-order)
		      (cons (coeff current-term)
			    (dense-iter (rest-terms terms)
					(order current-term))))
		     (else
		      (cons 0 (dense-iter terms (- prev-order 1)))))))))
    (dense-iter terms (+ (max-order terms) 1)))

  (define (max-order terms)
    (if (empty-termlist? terms)
	0
	(order (first-term terms))))
  (define (cardinality terms) (length-termlist terms))
  (define (neg terms)
    (map (lambda (term)
	   (make-term (order term)
		      (negate (coeff term))))
	 terms))
  (define (tag terms) (attach-tag 'sparse terms))

  ;;; Interface to outside

  (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(sparse) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(sparse) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(sparse) (lambda (terms)
				(tag (sparse-repr terms))))
  (put 'dense-repr '(sparse) (lambda (terms)
				 (attach-tag 'dense (dense-repr terms))))

  (put 'adjoin-term 'sparse (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(sparse) (lambda (terms) (cardinality terms)))
  (put 'max-order '(sparse) (lambda (terms) (max-order terms)))
  (put 'negate '(sparse) (lambda (p) (tag (neg p))))
  (put 'find-vars '(sparse) (lambda (terms) (find-v terms)))
  'done)

;; Dense polynomial package
;; each term only stores coefficient, order is implied by the
;; position in termlist
(define (install-dense-package)
  (define (find-v terms)
      (if (empty-termlist? terms)
	  (the-empty-termlist)
	  (let ((first (first-term terms)))	  
	    (cond ((poly? (coeff first)) (append (find-vars (coeff first)) (find-v (rest-terms terms))))
		  (else (find-v (rest-terms terms)))))))

  (define (zero-term? term) (= 0 (coeff term)))
  (define (max-order terms) (- (length terms) 1))
  (define (cardinality terms)
    (length (filter (lambda (x) (not (=zero? x))) terms)))
  (define (the-empty-termlist) '())
  (define (empty-termlist? terms) (null? terms))
  (define (first-term terms)
    (make-term (max-order terms) (car terms)))
  (define (add-term t1 t2)
    (make-term (order t1) (add (coeff t1) (coeff t2))))
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
    (cond ((empty-termlist? terms) (the-empty-termlist))
	  ((zero-term? (first-term terms)) (sparse-repr (rest-terms terms)))
	  (else (cons (first-term terms) (sparse-repr (rest-terms terms))))))
  (define (neg terms)
    (map (lambda (x) (negate x)) terms))
  (define (tag terms)
    (attach-tag 'dense terms))
  
  ;; Interface to outside
  (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(dense) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(dense) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(dense) (lambda (terms) (attach-tag 'sparse (sparse-repr terms))))
  (put 'dense-repr '(dense) (lambda (terms) (tag (dense-repr terms))))
  (put 'adjoin-term 'dense (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(dense) (lambda (terms) (cardinality terms)))
  (put 'max-order '(dense) (lambda (terms) (max-order terms)))
  (put 'negate '(dense) (lambda (terms) (tag (neg terms))))
  (put 'find-vars '(dense) (lambda (terms) (find-v terms)))
  'done)

;; function that installs all packages at once
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

;; run the installation
(install-packages)

;; prints off each term in a polynomial one term at a time (strictly for convenience).
(define (view-terms poly)
  (define (view-iter terms)
    (if (empty-termlist? terms)
	'()
	(begin
	  (display (first-term terms))
	  (newline)
	  (view-iter (rest-terms terms)))))
  (let ((terms (cdr (contents poly))))
    (view-iter terms)))



(define p1 (make-polynomial 'x (list 'sparse
				     (list 3 (make-polynomial 'y (list 'sparse
								       (list 2 4)
								       (list 1 (make-polynomial 'z (list 'sparse
													 (list 2 3))))))))))
(define p2 (make-polynomial 'y (list 'sparse
				     (list 2 (make-polynomial 'x (list 'sparse
								       (list 3 7))))
				     (list 1 (make-polynomial 'x (list 'sparse
									(list 1 (make-polynomial 'z (list 'sparse
													  (list 1 1)))))))
				     (list 0 7))))
p1
;Value 158: (polynomial x sparse (3 (polynomial y sparse (2 4) (1 (polynomial z sparse (2 3))))))
p2
;Value 160: (polynomial y sparse (2 (polynomial x sparse (3 7))) (1 (polynomial x sparse (1 (polynomial z sparse (1 1))))) (0 7))

(add p1 p2)
;Value 168: (polynomial x sparse (3 (polynomial y sparse (2 (polynomial z sparse (0 7))) (1 (polynomial z sparse (2 3))))) (3 (polynomial y sparse (2 (polynomial z sparse (0 4))))) (1 (polynomial y sparse (1 (polynomial z sparse (1 1))))) (0 (polynomial y sparse (0 (polynomial z sparse (0 7))))))



(define p1 (make-polynomial 'x '(sparse (1 1) (0 1))))
(define p2 (make-polynomial 'x '(sparse (3 1) (0 -1))))
(define p3 (make-polynomial 'x '(sparse (1 1))))
(define p4 (make-polynomial 'x '(sparse (2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

rf1
;Value 169: (rational (polynomial x sparse (1 -1) (0 -1)) polynomial x sparse (3 -1) (0 1))

rf2
;Value 170: (rational (polynomial x sparse (1 1)) polynomial x sparse (2 1) (0 -1))

(add rf1 rf2)
