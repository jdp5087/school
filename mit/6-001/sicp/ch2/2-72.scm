###2.72###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.72.  Consider the encoding procedure that you designed in exercise 2.68. What is the order of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps needed to search the symbol list at each node encountered. To answer this question in general is difficult. Consider the special case where the relative frequencies of the n symbols are as described in exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.
-----------------------------------------------------------------------------------------------------------------

(define (encode-symbol sym tree)
  (define (in-set? s set)
    (cond ((null? set) false)
	  ((eq? s (car set)) true)
	  (else (in-set? s (cdr set)))))
  (cond ((and (leaf? tree) (eq? sym (symbol-leaf tree))) '())
	((in-set? sym (symbols (left-branch tree))) (cons 0 (encode-symbol sym (left-branch tree))))
	((in-set? sym (symbols (right-branch tree))) (cons 1 (encode-symbol sym (right-branch tree))))
	(else (error "bad symbol -- ENCODE-SYMBOL " sym))))



;;; For convenience I have included my algorithm above. Note that the in-set? procedure checks every element in the worst case when
;;; the element being searched for is at the end of the set list. However, the huffman tree is always constructed so that the least frequent
;;; letters are stored first in the set.

;;; In order to search for the most-frequent letter, the in-set? procedure will have to do n-1 operations to check the left-branch
;;;, but it will only take 1 operation to check the right-branch. It will find the symbol in question during the next call, and the only
;;; remaining operations
;;; are constant time consing (cons 1 '()). So we have 2 calls to encode-symbol, where the first call results in n-1 + 1 operations, and the second
;;; results in c operations. thus the algorithm runs in f(n) = theta(n)

;;; The search for the least frequent symbol will take n calls to encode-symbol to find the symbol in question. On the first n-1 calls, 3 operations
;;; are required. The first is constant time, and the second is a call to in-set, and the third is a call to cons.
;;; but this will find the symbol at the beginning of the list every
;;; time. Because of this, we have (n)(c+c+c) and so f(n) = theta(n).
