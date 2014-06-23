;;; (drop (make-complex-from-real-imag 20 0))
;;; See if you can fix the fact that integers are coerced to real on project
;;; because of this drop doesn't work because an int will never be equal to the
;;; real that comes back up



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

(define (sin-generic x) (apply-generic 'sin x))
(define (cos-generic x) (apply-generic 'cos x))
(define (atan-generic x y) (apply-generic 'atan x y))
(define (square-generic x) (apply-generic 'square x))
(define (sqrt-generic x) (apply-generic 'sqrt x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define empty-set '())


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

(define *op-table* (make-hash-table equal?))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) false))


(define *coercion-types* (make-equal-hash-table))
(define *coercion-table* (make-equal-hash-table))

;; get and put functions to see a list of all types that can be converted TO
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

(define (construct-apply-coercion to-type)
  (lambda (from-object)
    (let ((from-type (type-tag from-object)))
      (if (equal? from-type to-type)
	  from-object
	  ((get-coercion-procedure from-type to-type) from-object)))))

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



(define (make-rational n d)
  ((get 'make 'rational) n d))

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

(define (make-real x)
  ((get 'make 'real) x))
	   

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

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


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

;;; COERCIONS NEEDED: atan square sqrt

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
(define (raise obj)
  (let ((obj-type (type-tag obj)))
    ((get-coercion-procedure obj-type (find-one-up obj-type)) obj)))


;; New definition of apply-generic
(define (apply-generic op . args)
  (define (apply-drop obj)
    (cond ((rat-function? obj) obj)
	  ((equal? (type-tag obj) (car (get-tower-hierarchy))) obj)
	  (else (let ((drop-result (drop obj)))
		  (if (equal? obj drop-result)
		      obj
		      (apply-drop drop-result))))))
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
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((apply-result (apply proc (map contents args))))
	    (if (not (member (type-tag apply-result) (get-tower-hierarchy)))
		apply-result
		(apply-drop apply-result)))
	  (if (all-types-equivalent? type-tags) ;; If all types are the same then a procedure doesn't exist
	      (error "No method found: all types are equivalent -- APPLY-GENERIC" (list op type-tags))
	      (let ((highest (find-highest-type type-tags))) ;; find the highest rank in type hierarchy in the current arguments
		(apply apply-generic (cons op (map (lambda (x)
						     (raise-to-rank x highest))
						   args))))))))) ;; raise every object to the highest rank

(define (project obj)
  (let ((obj-tag (type-tag obj)))
    (let ((reverse-hierarchy (reverse (get-tower-hierarchy))))
      (let ((projection (get-coercion-procedure obj-tag (find-one-down obj-tag))))
	(if projection
	    (projection obj)
	    (else "error -- projection not found for this procedure -- PROJECT -- " obj))))))

(define (drop obj)
  (if (equal? (raise (project obj)) obj)
      (project obj)
      obj))

(define (rat-function? obj)
  (if (not (equal? (type-tag obj) 'rational))
      false
      (let ((n (numer obj))
	    (d (denom obj)))
	(if (not (or (poly? n) (poly? d)))
	    false
	    true))))
	




























