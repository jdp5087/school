(define (element ptr)
  (car (car ptr)))
(define (backward ptr)
  (cdr (car ptr)))
(define (forward ptr)
  (cdr ptr))
(define (create-link e f b)
  (cons (cons

(define (front-ptr deque)
  (car deque))
(define (rear-ptr deque)
  (cdr deque))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))

  
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "called front deque on an empty deque " deque)
      (car (front-ptr deque)))

(define (make-deque)
  (cons '() '()))
(define (set-front-deque! deque item)
  (





