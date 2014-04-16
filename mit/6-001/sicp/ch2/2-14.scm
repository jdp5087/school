###2.14###
PROMPT:
------------------------------------------------------------
Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see exercise 2.12).
------------------------------------------------------------

(define (make-interval a b) (cons a b))
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

(let ((i1 (make-interval 6.12 7.48))
      (i2 (make-interval 4.47 4.94)))
  (add-interval i1 i2))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent center tolerance)
  (make-interval (- center (* center (/ tolerance 100)))
		 (+ center (* center (/ tolerance 100)))))
(define (percent i)
  (* (/ (width i) (center i)) 100))



(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

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

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(let ((a (make-center-percent 30 1))
      (b (make-center-percent 20 5)))
  (display a)
  (newline)
  (display b)
  (newline)
  (display (par1 a b))
  (newline)
  (display (par2 a b))
  (newline)
  (display (div-interval a a))
  (newline)
  (display (div-interval a b))
  (newline)
  (display "center: ")
  (display (center (par1 a b)))
  (newline)
  (display "percent: ")
  (display (percent (par1 a b)))
  (newline)
  (display "center: ")
  (display (center (par2 a b)))
  (newline)
  (display "percent: ")
  (display (percent (par2 a b)))
  (newline)
  (display "center: ")
  (display (center (div-interval b b)))
  (newline)
  (display "percent: ")
  (display (percent (div-interval b b)))
  (newline)
  (display "center: ")
  (display (center (div-interval a b)))
  (newline)
  (display "percent: ")
  (display (percent (div-interval a b))))









