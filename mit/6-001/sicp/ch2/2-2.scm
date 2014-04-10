(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (average s f)
  (/ (+ (f (start-segment s))
	(f (end-segment s)))
     2))
(define (midpoint s)
  (make-point (average s x-point)
	      (average s y-point)))

(let ((point1 (make-point 2 4))(point2 (make-point 4 0)))
  (let ((seg (make-segment point1 point2)))
    (print-point (midpoint seg))))