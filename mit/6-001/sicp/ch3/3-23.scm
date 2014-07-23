###3.23###
PROMPT:
------------------------------------------------------------
Exercise 3.23.  A deque (``double-ended queue'') is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations.23 All operations should be accomplished in (1) steps.
------------------------------------------------------------

(define (element ptr)
  (car (car ptr)))
(define (backward ptr)
  (cdr (car ptr)))
(define (forward ptr)
  (cdr ptr))
(define (make-link-front ele after)
  (cons (cons ele '()) after))
(define (make-link-rear ele before)
  (cons (cons ele before) '()))

(define (set-backward-ptr! ptr value)
  (set-cdr! (car ptr) value))
(define (set-forward-ptr! ptr value)
  (set-cdr! ptr value))

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (set-front-ptr! deque element)
  (set-car! deque element))
(define (set-rear-ptr! deque element)
  (set-cdr! deque element))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))
(define (make-deque)
  (cons '() '()))


(define (front-deque deque)
  (if (empty-deque? deque)
      (error "called front deque on an empty deque " deque)
      (element (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "called rear-deque on an empty deque " deque)
      (element (rear-ptr deque))))

(define (front-insert-deque! deque value)
  (if (empty-deque? deque)
      (let ((new (make-link-front value '())))
	(set-front-ptr! deque new)
	(set-rear-ptr! deque new)
	value)
      (let ((new (make-link-front value (front-ptr deque))))
	(set-backward-ptr! (front-ptr deque) new)
	(set-front-ptr! deque new)
	value)))

(define (rear-insert-deque! deque value)
  (if (empty-deque? deque)
      (let ((new (make-link-front value '())))
	(set-front-ptr! deque new)
	(set-rear-ptr! deque new)
	value)
      (let ((new (make-link-rear value (rear-ptr deque))))
	(set-forward-ptr! (rear-ptr deque) new)
	(set-rear-ptr! deque new)
	value)))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "front-delete-deque called on an empty deque " deque)
      (begin
	(set-front-ptr! deque (forward (front-ptr deque)))
	(if (not (null? (front-ptr deque)))
	    (set-backward-ptr! (front-ptr deque) '())))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "rear-delete-deque called on an empty deque " deque)
      (begin
	(set-rear-ptr! deque (backward (rear-ptr deque)))
	(if (not (null? (rear-ptr deque)))
	    (set-forward-ptr! (rear-ptr deque) '())))))




