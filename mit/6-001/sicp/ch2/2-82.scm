###2.82###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.82.  Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on. Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)
-----------------------------------------------------------------------------------------------------------------
;; This was a fun exercise. I decided to try to do better than just attempting to coerce all arguments to the first, then second, etc.
;; The weakness of that approach is that if a coercion for all needed types exists but isn't among the needed types, then they would not
;; be coerced to this type, and the program would fail uneccessarily. I ended up implementing a second hash table that tracks all types
;; that can be converted to. This hash table also tracks a set of all types that are available. This way, the get-coercion procedure
;; iterates all available to-coercions, and makes sure that every needed from-type is available for that to-type. This way, all possible
;; coercions are checked. This ends up being an expensive procedure, as there could be as many as (n-1)^2 relationships (every type but itself),
;; but it was fun. I would definitely implement sets a little more efficiently if this were production code.

;; I'm also learning that I need to find a better way to manage code between exercises, because a signficant portion of my time is spent
;; hunting down procedures that were defined 5 exercises or more ago. These are often the most elusive bugs. As the programs are becoming more
;; complex, I'm finding how much I took the convenience of python libraries and built-ins for granted. Not complaining, just saying I need
;; to find a way to adjust so that I'm not wasting time.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define empty-set '())

;;create two hash tables, one to track coercions, the other to track the types of coercions that are available
(define *coercion-types* (make-equal-hash-table))
(define *coercion-table* (make-equal-hash-table))

(hash-table/clear! *coercion-types*)
(hash-table/clear! *coercion-table*)

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
(get-coercion-procedure 'scheme-number 'complex)
(hash-table/get *coercion-table* '(scheme-number complex) '())
(hash-table/get *coercion-table* '(cat mouse) '())
(hash-table/key-list *coercion-table*)
((construct-apply-coercion 'complex) 8)
(hash-table->alist *coercion-table*)

;; New definition of apply-generic
(define (apply-generic op . args)
  (define (all-types-equivalent? types)
    (cond ((null? (cdr types)) true)
	  ((equal? (car types) (cadr types)) (all-types-equivalent? (cdr types)))
	  (else false)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (if (all-types-equivalent? type-tags) ;; If all types are the same then a procedure doesn't exist
	      (error "No method found: all types are equivalent -- APPLY-GENERIC" (list op type-tags))
	      (let ((coercion (get-coercion type-tags))) ; find a coercion type that can accomodate every needed type
		(if (not coercion)
		    (error "These objects cannot be coerced -- APPLY-GENERIC" (list op type-tags))
		    (let ((apply-coercion (construct-apply-coercion coercion))) ;; build the necessary coercion
		      (apply apply-generic (append (list op) (map apply-coercion args))))))))))) ;;apply apply-generic with coerced arguments



(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(add (make-complex-from-real-imag 3 4) 5)


