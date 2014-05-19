###2.61###
PROMPT:
------------------------------------------------------------
Exercise 2.61.  Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.
------------------------------------------------------------

(define (adjoin-set element set)
  (cond ((null? (cdr set)) (cons (car set) (cons element '())))
	((equal? element (car set)) set)
	((< element (cadr set)) (append (list (car set)) (list element) (cdr set)))
	(else (cons (car set) (adjoin-set element (cdr set))))))


(define (adjoin-set element set)
  (cond ((null? set) (cons element '()))
	((equal? element (car set)) set)
	((< element (car set)) (append (list element) set))
	(else (cons (car set) (adjoin-set element (cdr set))))))

(adjoin-set 5 '(1 2 3 5))
;Value 51: (1 2 3 5)

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
	((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
		      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		      ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3) '(3 4 5))
;Value 57: (1 2 3 4 5)
