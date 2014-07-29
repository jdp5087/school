###3.29###
PROMPT:
------------------------------------------------------------
Exercise 3.29.  Another way to construct an or-gate is as a compound digital logic device, built from and-gates and inverters. Define a procedure or-gate that accomplishes this. What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?
------------------------------------------------------------

(define (or-gate a1 a2 output)
  (let ((a (make-wire))
	(b (make-wire))
	(c (make-wire)))
    (inverter a1 a)
    (inverter a2 b)
    (and-gate a b c)
    (inverter c ouput)
    'ok))

;; The delay time for a change in either a1 and a2 to output from this compound digital logic gate is: 1. Inverter Delay, 2. And-gate Delay, 3. Inverter Delay