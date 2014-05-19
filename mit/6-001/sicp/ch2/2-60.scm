###2.60###
PROMPT:
------------------------------------------------------------
Exercise 2.60.  We specified that a set would be represented as a list with no duplicates. Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and intersection-set that operate on this representation. How does the efficiency of each compare with the corresponding procedure for the non-duplicate representation? Are there applications for which you would use this representation in preference to the non-duplicate one?
------------------------------------------------------------

(define (element-of-set element set)
  (cond ((null? set) false)
	((equal? element (car set)) true)
	(else (element-of-set element (cdr set)))))

(define (adjoin-set element set)
  (cons element set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set (car set1) set2)
	 (adjoin-set (car set1)
		     (intersection-set (cdr set1)
				       set2)))
	(else (intersection-set (cdr set1) set2))))

(define set1 '(2 3 2 1 3 2 2))
(define set2 '(8 8 9 3 4 2))

(intersection-set set1 set2)
;Value 33: (2 3 2 3 2 2)
(union-set set1 set2)


;;; The insertions are much faster in this implementation, because they occur in constant time as opposed to linear time.
;;; Lookups, on the other hand, are going to be slower in data with a lot of repeated values, this is all dependant on the data.
;;; union-set is runs in linear time proportional to the size of set 1. this is just an append operation. Intersection set is
;;; virtually the same as before, having n^2 complexity.

;;; So it all depends on the intended use of a program. Many insertions, few lookups would favor this implementation, where
;;; Few insertions, many lookups would heavily favor the other.
