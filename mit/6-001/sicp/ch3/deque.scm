(define (element ptr)
  (car (car ptr)))
(define (backward ptr)
  (cdr (car ptr)))
(define (forward ptr)
  (cdr ptr))
(define (make-link-front ele after)
  (cons (cons ele '()) after))
(define (make-link-back ele before)
  (cons (cons ele before) '()))

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
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
	(set-car! deque new)
	(set-cdr! deque new)
	value)
      (begin
	(set-car! deque (make-link-front value (front-ptr deque)))
	value)))

(define (back-insert-deque! deque value)
  (if (empty-deque? deque)
      (let ((new (make-link-front value '())))
	(set-car! deque new)
	(set-cdr! deque new))
      (begin
	(set-cdr! deque (make-link-back value (rear-ptr deque)))
	value)))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "front-delete-deque called on an empty deque " deque)
      (set-car! deque (forward (front-ptr deque)))))

(define (back-delete-deque! deque)
  (if (empty-deque? deque)
      (error "back-delete-deque called on an empty deque " deque)
      (set-cdr! deque (backward (rear-ptr deque)))))

		
(define d (make-deque))

(front-insert-deque! d '1)
(front-insert-deque! d '2)
(front-insert-deque! d '3)
(back-insert-deque! d '0)

(front-deque d)
(rear-deque d)
(front-delete-deque! d)
(back-delete-deque! d)




;;; change so that all references to back change to rear

;;; after adding second element, backward of rear-ptr should point to next element

;; or consider just making a circular reference on null, however this seems like it could complicate things.
