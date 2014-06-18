###2.84###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.84.  Using the raise operation of exercise 2.83, modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising, as discussed in this section. You will need to devise a way to test which of two types is higher in the tower. Do this in a manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels to the tower.
-----------------------------------------------------------------------------------------------------------------

;; New definition of apply-generic
(define (apply-generic op . args)
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
          (apply proc (map contents args))
	  (if (all-types-equivalent? type-tags) ;; If all types are the same then a procedure doesn't exist
	      (error "No method found: all types are equivalent -- APPLY-GENERIC" (list op type-tags))
	      (let ((highest (find-highest-type type-tags))) ;; find the highest ranked element of the current arguments
		(apply apply-generic (cons op (map (lambda (x)
						     (raise-to-rank x highest))
						   args))))))))) ;; raise every object to the highest rank and call apply-generic



(add 1 (make-complex-from-real-imag 4 3))
;Value 75: (complex rectangular 5. . 3)
