

(define (forward-queue deque)
  (car deque))
(define (backward-queue deque)
  (cdr deque))
(define (front-ptr queue)
  (car queue))
(define (back-ptr queue)
  (cdr queue))
(define (front-ptr-forward deque)
  (front-ptr (forward-queue deque)))
(define (back-ptr-forward deque)
  (back-ptr (forward-queue deque)))
(define (front-ptr-backward deque)
  (front-ptr (backward-queue deque)))
(define (back-ptr-backward deque)
  (back-ptr (backward-queue deque)))

(define (insert-front-deque! deque element)
  (if (empty-deque? deque)
      (let ((new (cons element '())))
	(set-forward-front! deque new)
	(set-forward-back! deque new)
	(set-backward-front! deque new)
	(set-backward-back! deque new))
      (begin
	(set-forward-front! deque (cons element (front-ptr-forward deque)))
	(set-backward-back! deque (cons element '())))))
(define (insert-back-deque! deque element)
  (if (empty-deque deque)
      (let ((new (cons element '())))
	(set-forward-front! deque new)
	(set-forward-back! deque new)
	(set-backward-front! deque new)
	(set-backward-back! deque new))
      (begin
	(set-backward-front! deque (cons element (front-ptr-backward deque)))
	(set-forward-back! deque (cons element '())))))

(define (delete-front-queue! deque)
  (if (empty-deque? deque)
      (error "attempted to delete an element from an empty deque -- DELETE-FRONT-QUEUE!")
      (begin
	(set-forward-front! deque (cdr (front-ptr-forward deque)))
	(set-backward-back! deque '()))))
(define (delete-front-queue! deque)
  (if (empty-deque? deque)
      (error "attempted to delete an element from an empty deque -- DELETE-FRONT-QUEUE!")
      (begin
	(set-backward-front! deque (cdr (front-ptr-forward deque)))
	(setf-forward-back! deque (cons (

(define (empty-queue? deque)
  (or (null? (front-ptr-forward deque))
      (null? (back-ptr-forward deque
