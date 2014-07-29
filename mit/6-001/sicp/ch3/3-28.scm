###3.28###
PROMPT:
------------------------------------------------------------
Exercise 3.28.  Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate. 
------------------------------------------------------------

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
	((and (or (= s1 0) (= s1 1)) (or (= s2 0) (= s2 1))) 1)
	(else (error "Invalid signal " (list s1 s2)))))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
