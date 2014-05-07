;;; vector functions

(define (make-vect x y)
  (cons x y))
(define (vector-xcor v)
z  (car v))
(define (vector-ycor v)
  (cdr v))
(define (vector-add v w)
  (cons (+ (xcor-vect v) (xcor-vect w))
	(+ (ycor-vect v) (ycor-vect w))))
(define (vector-sub v w)
  (cons (- (xcor-vect v) (xcor-vect w))
	(- (ycor-vect v) (ycor-vect w))))
(define (vector-scale v s)
  (cons (* (xcor-vect v) s)
	(* (ycor-vect v) s)))


;;; frame functions

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (car (cdr frame)))
(define (frame-edge2 frame)
  (car (cdr (cdr frame))))


;;; segment functions

(define (make-segment start end)
  (cons start end))
(define (segment-start segment)
  (car segment))
(define (segment-end segment)
  (cdr segment))



(load "paint-lib.com")