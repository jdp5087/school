###3.18###
PROMPT:
-----------------------------------------------------------
Exercise 3.18.  Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.
------------------------------------------------------------
(define (cycle? struct)
  (define (check-upstream pointer up)
    (cond ((null? up) false)
	  ((eq? pointer (car up)) true)
	  (else (check-upstream pointer (cdr up)))))
  (define (check-for-cycle s k)
    (cond ((null? k) false)
	  ((and (eq? s (car k))
		(pair? s)
		(or (check-upstream (car s) (cdr k))
		    (check-upstream (cdr s) (cdr k))))
	   true)
	  (else (check-for-cycle s (cdr k)))))
  (let ((known '()))
    (define (iter s)
      (cond ((null? s) (list false))
	    ((check-for-cycle s known) (list true))
	    ((not (pair? s)) (list false))
	    (else
	     (begin (set! known (cons s known))
		    (append (iter (car s)) (iter (cdr s)))))))
    (reduce-left (lambda (x y)
		   (if (and (false? x) (false? y))
		       false
		       true))
		 false
		 (iter struct))))
     
	   


(define w (list 'a 'b 'c))
(cycle? w)
;Value: #f

(define f (cons 'a '()))
(define s (cons 'b f))
(set-cdr! (cdr s) s)
(cycle? s)
;Value: #f
