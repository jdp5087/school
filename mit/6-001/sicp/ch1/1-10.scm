(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))



(A 1 10)
(A 0
   (A 1 9))
(A 0
   (A 0
      (A 1 8)))
(A 0
   (A 0
      (A 0
	 (A 1 7))))
;;; until...
(A 0
   (A 0
      (A 0
	 (A 0
	    (A 0
	       (A 0
		  (A 0
		     (A 0
			(A 0
			   (A 1 1))))))))))
;;; which returns 2
;;; this doubles 2 9 times (which is 2^10)
;;; this evaluates to 1024

(A 2 4)
(A 1
   (A 2 3))
(A 1
   (A 1
      (A 2 2)))
(A 1
   (A 1
      (A 1
	 (A 2 1))))
;;; This returns 2
(A 1
   (A 1
      (A 1
	 2)))
(A 1
   (A 1
      (A 0
	 (A 1 1))))
(A 1
   (A 1
      (A 0
	 2)))
(A 1
   (A 1
      4))
(A 1
   (A 0
      (A 1 3)))
;;; ...until

(A 1
   (A 0
      (A 0
	 (A 0 1))))
; which is 2^4

(A 1 16)
;;; which is 2^16
;;; that results in 65536


(A 3 3)
(A 2
   (A 3 2))
(A 2
   (A 2
      (A 3 1)))
(A 2
   (A 2
      2))
(A 2
   (A 2
      2))
(A 2
   (A 1
      (A 2 1)))
(A 2
   (A 1
      2))
(A 2
   (A 0
      (A 1 1)))
(A 2
   (A 0
      2))
(A 2
   4)
;;; which we know returns 65536


(A 2 0)
(A 2 1)
(A 2 2)
(A 2 3)
(A 2 4)
(A 2 5)

(* 256 256)




	 





