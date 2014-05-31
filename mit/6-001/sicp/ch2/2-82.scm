###2.82###
PROMPT:
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define empty-set '())

(define *coercion-types* (make-strong-eq-hash-table))
(define *coercion-table* (make-strong-eq-hash-table))

(define (add-type-to-coercion-types type)
  (hash-table/put! *coercion-types*
		   'all-types
		   (adjoin-set type
			       (hash-table/get *coercion-table*
					       'all-types
					       empty-tree))))
(define (get-all-coercion-types)
  (hash-table/get *coercion-table* 'all-types empty-set))
(define (get-all-coercion-types-list)
  (hash-table/get *coercion-table* 'all-types empty-set))
(define (put-new-type type)
  (hash-table/put! *coercion-table* 'all-types (adjoin-set type (get-coercion-types))))

(define (get-coercion-type type)
  (hash-table/get *coercion-types* type empty-set))
(define (put-coercion-type from-type to-type)
  (hash-table/put! *coercion-types* from-type (adjoin-set to-type (get-coercion-type from-type))))
(define (put-coercion from-type to-type proc)
  (add-type-to-coercion-types from-type)
  (put-coercion-type from-type to-type)
  (hash-table/put! *coercion-table* (list from-type to-type) proc))
(define (get-coercion from-type to-type)
  (hash-table/get *coercion-table* (list from-type to-type) false))

(put-coercion 'dog 'mouse (lambda (x y) (y)))
(get-coercion-type 'dog)
(car (hash-table->alist *coercion-types*))
(hash-table/datum-list *coercion-types*)

(get-coercion-types)

(define (get-coercion needed-types)
  (define (type-in-list? t l)
    (cond ((null? l) false)
	  ((eq? t (car l)) true)
	  (else (type-in-list? t (cdr l)))))
  (define (iter-types types)
    (cond ((null? types) false)
	  ((viable? (car types) needed-types) (car types))
	  (else (iter-types (cdr types)))))
  (define (viable? type needed)
    (cond ((null? needed) true)
	  ((element-of-set? (car needed) (get-coercion-type type)) (viable? type (cdr needed)))
	  (else false)))
  (let ((all-coercion-types (get-all-coercion-types)))
    (iter-types all-coercion-types)))


    
(get-coercion (list 'mouse))
 