###2.63###
PROMPT:
------------------------------------------------------------
Exercise 2.63.  Each of the following two procedures converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

a. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in figure 2.16?

b. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?
------------------------------------------------------------

a. It appears that the lists will both yield lists that are in the same order. The second algorithm is much harder to interpret, but
   is quite elegant.

In the first algorithm, the left side is always appended to as the front side to the entry cons'ed on to the right side. The list
will always be from left-to-right across the leaves.

The same is true of the second algorithm. This one sends to end of list sentinel all the way down the right side, and the result of each
recursion is then attached after the contents of the left side because it is passed as result-list to the call of copy-to-list of the left-side.
Very interesting algorithm.

b. The first algorithm must traverse the list every time as appends. Therefore, it will have to traverse the list passed from the left side
   every time an append is called. This list doubles in size every step it is passed up, for a total of log-n steps. Since the algorithm touches
   every node once, the run time is nlogn

  The second algorithm uses deferred operations to avoid the need for appends. it has an n running time, assuming cons is a constant-time
  operation.



(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))



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

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))



(define tree1 (make-tree 5
			 (make-tree 3 (make-tree 2 '() '()) (make-tree 4 '() '()))
			 (make-tree 7 (make-tree 6 '() '()) (make-tree 8 '() '()))))


(tree->list-1 tree1)
;Value 60: (2 3 4 5 6 7 8)

(tree->list-2 tree1)
;Value 61: (2 3 4 5 6 7 8)

(quotient 21 2)