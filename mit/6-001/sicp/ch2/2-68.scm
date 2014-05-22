###2.68###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.68.  The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encode-symbol so that it signals an error if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.
-----------------------------------------------------------------------------------------------------------------

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol sym tree)
  (define (in-set? s set)
    (cond ((null? set) false)
	  ((eq? s (car set)) true)
	  (else (in-set? s (cdr set)))))
  (cond ((and (leaf? tree) (eq? sym (symbol-leaf tree))) '())
	((in-set? sym (symbols (left-branch tree))) (cons 0 (encode-symbol sym (left-branch tree))))
	((in-set? sym (symbols (right-branch tree))) (cons 1 (encode-symbol sym (right-branch tree))))
	(else (error "bad symbol -- ENCODE-SYMBOL " sym))))


(encode '(a d a b b c q) sample-tree)
;bad symbol -- ENCODE-SYMBOL  q
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

(encode '(a d a b b c a) sample-tree)
;Value 19: (0 1 1 0 0 1 0 1 0 1 1 1 0)



