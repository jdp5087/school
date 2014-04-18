PROMPT:
------------------------------------------------------------
Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as


where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown above.
------------------------------------------------------------

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons fn a b n)
  (let ((start a)
	(h (/ (- b a) n)))
    (define (add-h a)
      (+ a h))
    (define (get-k a)
      (/ (- a start) h))
    (define (f-wrapper a)
      (cond ((or (= (get-k a) 1) (= (get-k a) n)) (* 1.0 (fn a)))
	    ((= (remainder (round (get-k a)) 2) 1) (* 4.0 (fn a)))
	    (else (* 2.0 (fn a)))))
    (* (sum f-wrapper (+ a h) add-h b)
       (/ h 3))))

(integral cube 0.0 1.0 0.01)
;Value: .24998750000000042
(integral cube 0.0 1.0 0.001)
;Value: .249999875000001

      
(simpsons cube 0 1 100)
; WITH INT Value: .24999999
; WITH FLOAT Value: .2499999899999999

(simpsons cube 0 1 1000)
; WITH INT Value: .249999999999
; WITH FLOAT Value: .24999999999900022

;;Notice that the floating point multiplications in the cond statement lead to decreased accuracy in Simpson's
;;If the input a is an integer and integers are used in the multiplication, we have greater accuracy


