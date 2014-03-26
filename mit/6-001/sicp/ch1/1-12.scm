(define (pascal row column)
   (cond ((or (= column row) (= 1 column)) 1)
	 (else (+ (pascal (- row 1) (- column 1))
		  (pascal (- row 1) column)))))

(pascal 4 3)







