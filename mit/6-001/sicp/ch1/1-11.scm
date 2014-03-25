;;; linear recursive process

(define (f n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;;; linear iterative process

(define (g-iter x y z count n)
  (cond ((< n 3) n)
	((< count 3) x)
	(else (g-iter (+ x (* 2 y) (* 3 z))
		      x
		      y
		      (- count 1)
		      n))))

(define (g n)
  (g-iter 2 1 0 n n))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)

(g 0)
(g 1)
(g 2)
(g 3)
(g 4)
(g 5)
(g 6)
(g 7)
  