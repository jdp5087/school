###3.34###
PROMPT:
------------------------------------------------------------
Exercise 3.34.  Louis Reasoner wants to build a squarer, a constraint device with two terminals such that the value of connector b on the second terminal will always be the square of the value a on the first terminal. He proposes the following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

There is a serious flaw in this idea. Explain. 
------------------------------------------------------------
(define (squarer a b)
  (multiplier a a b)
  'ok)

(define test1 (make-connector))
(define test2 (make-connector))

(squarer test1 test2)

(probe "in-wire" test1)
(probe "out-wire" test2)

(set-value! test1 2 'user)
Probe: in-wire = 2
Probe: in-wire = 2
Probe: out-wire = 4
Probe: out-wire = 4
;Value: done

(forget-value! test1 'user)

(set-value! test2 4 'user)
Probe: out-wire = 4
Probe: out-wire = 4
;Value: done

;; The first problem is that a change to wire a triggers the constraint to calculate its value twice.

;; The second problem is that a call to set-value! on the output terminal cannot implement a square root function, becuase
;; with only one input, it doesn't have enough information to do so. This defeats the purpose of a constraints system,
;; which has its main advantage in the fact that it is multidirectional.






