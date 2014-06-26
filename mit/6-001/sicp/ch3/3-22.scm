###3-22###
PROMPT:
--------------------------------------------------------------------------------
Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form

(define (make-queue)
  (let ((front-ptr ...)
        (rear-ptr ...))
    <definitions of internal procedures>
    (define (dispatch m) ...)
    dispatch))

Complete the definition of make-queue and provide implementations of the queue operations using this representation.
--------------------------------------------------------------------------------\

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  '()
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
      (if (empty-queue?)
	  (begin
	    (set-front-ptr! new-pair)
	    (set-rear-ptr! new-pair)
	    front-ptr)
	  (begin
	    (set-cdr! rear-ptr new-pair)
	    (set-rear-ptr! new-pair)
	    front-ptr))))
    (define (delete-queue!)
      (if (empty-queue?)
	  (error "DELETE! Called with an empty queue " queue)
	  (begin (set-front-ptr! (cdr front-ptr))
		 front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    (else (error "unknown operation -- MAKE-QUEUE " m))))
    dispatch))

(define (empty-queue? obj)
  ((obj 'empty-queue?)))
(define (front-queue obj)
  ((obj 'front-queue)))
(define (insert-queue! obj item)
  ((obj 'insert-queue!) item))
(define (delete-queue! obj)
  ((obj 'delete-queue!)))

(define q (make-queue))

(empty-queue? q)
;Value: #t

(front-queue q)
;Value: ()

(insert-queue! q 'a)
(insert-queue! q 'b)

(front-queue q)
;Value: a

(delete-queue! q)

(front-queue q)
;Value: b


