###3.31###
PROMPT:
------------------------------------------------------------
Exercise 3.31.   The internal procedure accept-action-procedure! defined in make-wire specifies that when a new action procedure is added to a wire, the procedure is immediately run. Explain why this initialization is necessary. In particular, trace through the half-adder example in the paragraphs above and say how the system's response would differ if we had defined accept-action-procedure! as

(define (accept-action-procedure! proc)
  (set! action-procedures (cons proc action-procedures)))
------------------------------------------------------------

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
 
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
sum 0  New-value = 0
(probe 'carry carry)
carry 0  New-value = 0

;; Notice that when (probe 'sum sum) and (probe 'carry carry) are called, that the interpreter returns the message that probe is meant to display. This is because the action is being called immediately. This is entirely inconsequential in the case of calls to (probe)

;; However, when the actions of and-gates, or-gates, and inverters are added to their input wires, these gates must propogate their signals to the wires that come after
;; in sequence. This is because if these actions aren't called, the value of an output wire would be based on all 0's. So the actions will not be triggered on initialization.
;; This could lead to wires with wrong outputs, because the wires were never correctly set to their initial values.