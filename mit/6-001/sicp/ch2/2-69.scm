###2.69###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.69.  The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)
-----------------------------------------------------------------------------------------------------------------
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))




(define (successive-merge pairs)
  (define (merge-iter leaf-set)
    (if (null? (cddr leaf-set))
	leaf-set
	(let ((c (car leaf-set))
	      (d (cadr leaf-set)))
	  (merge-iter (adjoin-set (make-code-tree c d) (cddr leaf-set))))))
  (merge-iter pairs))

(define leaf-set '((A 8) (Q 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(generate-huffman-tree leaf-set)
;Value 12: (((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf q 3) (d c q) 5) (h g f e d c q) 9) (a h g f e d c q) 17))



  



