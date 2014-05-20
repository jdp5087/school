###2.64###
PROMPT:
------------------------------------------------------------
Exercise 2.64.  The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.

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

a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

b. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?
------------------------------------------------------------

;; Wow, this is neat. The list->tree takes a list of elements, and calls partial tree with elements and length of elements, as stated above.
;; Partial tree must be called with a list of elements and n being at most the length of elements.

;; If the n is size 0, the procedure returns an empty list. This will occur when the list above was either size 1 or 2. If calling procedure had
;; n=2, an empty node will be attached on the left, and a node with two empty nodes under it will be attached to the right, which is because
;; the call to partial-tree on the right had n=1, which makes a tree with the current entry with two empty leaves underneath.

;; Now that we have covered the two base cases, any case above will find the size of one less than half of the list. On even sized lists this results
;; in the entry being the element at left center, otherwise the entry is the exact center. the list will be passed to partial-tree on the left and n
;; being the size of everything left of entry in the list. Whatever comes back up is a pair. The car of this pair is the tree to be attached on the left.
;; The procedure then prescribes that the value in car of the not used values becomes the entry. Then it calls partial-tree on the cdr of the used-value,
;; making a tree of all of the values on the right. note that the cdr of the pair from partial-tree will be empty coming back from the right, and
;;  thus is ignored.

;; Assuming that cons and car and arithmetic is constant time, the only procedure that will weigh on the asymptotic complexity is partial-tree.
;; We see that on every call to partial-tree, there are two calls to partial tree, and the size of each call is either n/2 or n-1/2. Therefore we will
;; see 2^log(n) complexity, or simply n.