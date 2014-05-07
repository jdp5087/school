###2.47###
PROMPT:
------------------------------------------------------------
Exercise 2.47.  Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

For each constructor supply the appropriate selectors to produce an implementation for frames.
------------------------------------------------------------

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(let ((origin (make-vect 2 2))
      (e1 (make-vect 0 1))
      (e2 (make-vect -1 2)))
  (let ((frame (make-frame origin e1 e2)))
    (display (origin-frame frame))
    (display (edge1-frame frame))
    (display (edge2-frame frame))))

;(2 . 2)(0 . 1)(-1 . 2)
;Unspecified return value
