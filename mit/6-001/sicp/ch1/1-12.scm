(define (pascal row column)
   (cond ((or (= column row) (= 1 column)) 1)
	 (else (+ (pascal (- row 1) (- column 1))
		  (pascal (- row 1) column)))))

(pascal 4 3)

(+ (pascal 3 2) (pascal 3 3))
(pascal 3 2)

(+ (pascal 3 2) 1)
(+ (+ (pascal 2 1) (pascal 2 2)) 1)
(+ (+ 1 1) 1)
(+ 2 1)
3



(or (< 1 2) (> 3 2))



