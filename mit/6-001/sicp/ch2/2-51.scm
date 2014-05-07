###2.51###
PROMPT:
------------------------------------------------------------
Exercise 2.51.  Define the below operation for painters. Below takes two painters as arguments. The resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second painter in the top. Define below in two different ways -- first by writing a procedure that is analogous to the beside procedure given above, and again in terms of beside and suitable rotation operations (from exercise 2.50).
------------------------------------------------------------

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
	   (transform-painter painter
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0)))
	  (paint-bottom
	   (transform-painter painter
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
      (lambda (frame)
	(paint-top frame)
	(paint-bottom frame)))))

;;; and here's the second version using definitions from 2.50

(define (below painter1 painter2)
  (let ((paint-below (beside (rotate90 (rotate270 painter1)
				       (rotate270 painter2)))))
    (lambda (frame) (paint-below frame))))

