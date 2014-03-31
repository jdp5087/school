(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))


(sin 12.15)
(if (not (> (abs 12.15)) 0.1)
    12.15
    (p (sine (/ 12.15 3.0))))

(if (not #t)
    12.15
    (p (sine (/ 12.15 3.0))))

(if #f
    12.15
    (p (sine(/ 12.15 3.0))))

(p (sine(/ 12.15 3.0)))

(p (sine 4.05))

(p
 (if (not (> (abs 4.05)) 0.1)
    12.15
    (p (sine (/ 4.05 3.0)))))

(p
 (if (not #t))
    4.05
    (p (sine (/ 4.05 3.0)))))

(p
 (if #f)
    4.05
    (p (sine (/ 4.05 3.0)))))

(p (p (sine (/ 4.05 3.0)))))

(p (p (sine (/ 1.35 3.0)))))

(p (p (sine 1.35))))

(p (p
    (if (not (> (abs 1.35)) 0.1)
	1.35
	(p (sine (/ 1.35 3.0))))))

(p (p
    (if (not  #t)
	1.35
	(p (sine (/ 1.35 3.0))))))

(p (p
    (if #f
	1.35
	(p (sine (/ 1.35 3.0))))))

(p (p (p (sine (/ 1.35 3.0))))))

(p (p (p (sine 0.45)))))

(p (p (p
       (if (not (> (abs 0.45)) 0.1)
	   0.45
	   (p (sine (/ 0.45 3.0)))))))

(p (p (p
       (if (not #t)
	   0.45
	   (p (sine (/ 0.45 3.0)))))))

(p (p (p
       (if #f
	   0.45
	   (p (sine (/ 0.45 3.0)))))))

(p (p (p (p (sine 0.15))))))

(p (p (p (p
	  (if (not #t)
	      0.15
	      (p (sine (/ 0.15 3.0))))))))

(p (p (p (p
	  (if #f
	      0.15
	      (p (sine (/ 0.15 3.0))))))))

(p (p (p (p (p (sine (/ 0.15 3.0)))))))

(p (p (p (p (p (sine 0.05))))))

(p (p (p (p (p
	     (if (not (> (abs 0.05)) 0.1)
		 0.05
		 (p (sine (/ 0.05 3.0)))))))))

(p (p (p (p (p
	     (if (not #f)
		 0.05
		 (p (sine (/ 0.05 3.0)))))))))

(p (p (p (p (p
	     (if #t
		 0.05
		 (p (sine (/ 0.05 3.0)))))))))

;;;Fast forward version from here out...

(p (p (p (p (p 0.05)))))

(p (p (p (p 0.1495))))

(p (p (p 0.4351)))

(p (p 0.9758))

(p -0.7892)

-0.4014

;;;this is rounded, actual value of (sine 12.15) is: ;Value: -.39980345741334







