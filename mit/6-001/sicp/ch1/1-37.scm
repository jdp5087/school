###1.37###
PROMPT:
------------------------------------------------------------
@@@ Most questions that contain formulas don't copy over properly, see SICP for the formulas @@@

Exercise 1.37.  a. An infinite continued fraction is an expression of the form


As an example, one can show that the infinite continued fraction expansion with the Ni and the Di all equal to 1 produces 1/, where  is the golden ratio (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form


Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/ using

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?

b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
------------------------------------------------------------

(define (cont-frac n d k)
  (define (cont-frac-iter n d k i)
    (if (= k i)
	(d i)
	(/ (n i)
	   (+ (d i)
	      (cont-frac-iter n d k (+ i 1))))))
  (cont-frac-iter n d k 1))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   1)
;Value: 1.

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   10)
;Value: .6179775280898876

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   100)
;Value: .6180339887498948

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   1000)
;Value: .6180339887498948

A k-value of 11 gives an accuracy of 4 decimal places.


;;; PART B, iterative procedure
(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
	result
	(cont-frac-iter (- i 1)
			(/ (n i)
			   (+ (d i) result)))))
  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   10)
;Value: .6179775280898876




