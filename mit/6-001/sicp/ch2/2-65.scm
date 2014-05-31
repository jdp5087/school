###2.65###
PROMPT:
------------------------------------------------------------
Exercise 2.65.  Use the results of exercises 2.63 and  2.64 to give (n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.41
------------------------------------------------------------
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define empty-tree '())
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;;;;;;;;;FIRST IMPLEMENTATION:::::::
(define (intersection-set set1 set2)
  (define (check-member element set)
    (if (element-of-set? element set2)
	(adjoin-set element set)
	set))
  (define (traverse-tree tree results)
    (if (null? tree)
	results
	(traverse-tree (left-branch tree)
		       (check-member (entry tree)
				     (traverse-tree (right-branch tree)
						    results)))))
  (list->tree (tree->list (traverse-tree set1 empty-tree))))

;;; Note the previous algorithm for intersection set is nlgn because of the calls to adjoin-set and element-of-set for every call to a subproblem
;;; so we have lgn*(cn) + 2lgn(cn) + 4lgn(cn), ..., lgn*lgn*(cn) = c*cn*lgn ~ nlgn complexity.


(define (intersection-set s1 s2)
  (define (intersection-set-iter set1 set2)
    (if (or (null? set1) (null? set2))
	'()    
	(let ((x1 (car set1)) (x2 (car set2)))
	  (cond ((= x1 x2)
		 (cons x1
		       (intersection-set-iter (cdr set1)
					      (cdr set2))))
		((< x1 x2)
		 (intersection-set-iter (cdr set1) set2))
		((< x2 x1)
		 (intersection-set-iter set1 (cdr set2)))))))
  (list->tree (intersection-set-iter (tree->list s1) (tree->list s2))))

(define (union-set s1 s2)
 (define (union-set-iter set1 set2)
   (cond ((and (null? set1) (null? set2)) '())
	 ((null? set1) set2)
	 ((null? set2) set1)
	 (else (let ((x1 (car set1))
		     (x2 (car set2)))
		 (cond ((= x1 x2) (cons x1 (union-set-iter (cdr set1) (cdr set2))))
		       ((< x1 x2) (cons x1 (union-set-iter (cdr set1) set2)))
		       ((< x2 x1) (cons x2 (union-set-iter set1 (cdr set2)))))))))
 (list->tree (union-set-iter (tree->list s1) (tree->list s2))))

(define s1 (list->tree (list 1 2 3 4 5 6)))
(define s2 (list->tree (list 2 3 4 5 6 7)))

(union-set s1 s2)
;Value 37: (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))

(intersection-set s1 s2)
;Value 38: (4 (2 () (3 () ())) (5 () (6 () ())))

;;; Both of these implemenations are n in run-time with respect to inputs because we have tree->list twice, union-set-iter or intersection-set-iter 
;;; once, and list->tree once.

;;; all of these algorithms are seprately n complexity, so when we add them we have n + n + n + n = 4n ~ n





