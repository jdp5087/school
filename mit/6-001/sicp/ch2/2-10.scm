PROMPT:
------------------------------------------------------------
Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.
------------------------------------------------------------

The problem of dividing by an interval that spans 0 is that an interval describes merely where a number could occur. Thus, the number could in fact be 0.
Our division procedure is allowing a potential division by zero error to procede uninhibited. This should return an undefined, so let's see what that would
look like.

(define (div-interval x y)
  (if (and (>= (upper-bound x) 0) (<= (lower-bound y) 0))
      (error "Division by an interval that spans 0 is undefined:" y)
      (mul-interval x 
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

(let ((i1 (make-interval 6.12 7.48))
      (i2 (make-interval -4.47 4.94)))
  (div-interval i1 i2))

