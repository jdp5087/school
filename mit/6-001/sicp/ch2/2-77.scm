###2.77###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.77.  Louis Reasoner tries to evaluate the expression (magnitude z) where z is the object shown in figure 2.24. To his surprise, instead of the answer 5 he gets an error message from apply-generic, saying there is no method for the operation magnitude on the types (complex). He shows this interaction to Alyssa P. Hacker, who says ``The problem is that the complex-number selectors were never defined for complex numbers, just for polar and rectangular numbers. All you have to do to make this work is add the following to the complex package:''

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

Describe in detail why this works. As an example, trace through all the procedures called in evaluating the expression (magnitude z) where z is the object shown in figure 2.24. In particular, how many times is apply-generic invoked? What procedure is dispatched to in each case?
-----------------------------------------------------------------------------------------------------------------

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(magnitude z)

(apply-generic 'magnitude z)

(apply-generic 'magnitude . z)

(let ((type-tags ('(complex))))
    (let ((proc (get 'magnitude '(complex)))) ;; This returns magnitude
      (if proc
          (apply proc ('rectangular . (3 . 4)))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(magnitude ('rectangular . (3 . 4)))

(apply-generic 'magnitude . ('rectangular . (3 . 4)))

(let ((type-tags ('rectangular))))
  (let ((proc (get 'magnitude ('rectangular)))) ;; This time the procedure will take rectangular and apply the value stored in our table by magnitude
    (if proc
	(apply proc (3 . 4)))) ;; So now we go to the scope of the rectangle magnitude function

;; within rectangle package
(magnitude (3.4))
(sqrt (+ (square (real-part (3 . 4)))
	 (square (imag-part (3 . 4)))))

(sqrt (+ (square 3)
	 (square 4)))

(sqrt (+ 9
	 16))

(sqrt 25)

5


;; Alyssa's suggestion is correct. The reason that this works is because every call to (apply-generic) strips off a type-tag. Thus, but having an
;; operation tagged 'magnitude with the type ('complex), the procedure simply strips off the ('complex) tag, on the assumption that a tag under
;; will indicate whether the data is stored as ('rectangular) or ('polar). The 'magnitude function stored in our put table under the type ('complex)
;; merely passes the function downwards and allows the remaining tags to funnel the data towards the correct procedures. Very interesting concept.

;; By the way, apply-generic was called twice, in response to the prompt.

