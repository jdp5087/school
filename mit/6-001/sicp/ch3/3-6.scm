###3.6###
-----------------------------------------------------------------------------------------------------------------
Exercise 3.6.  It is useful to be able to reset a random-number generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument that is either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number; ((rand 'reset) <new-value>) resets the internal state variable to the designated <new-value>. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use random numbers.
-----------------------------------------------------------------------------------------------------------------


;; Note that I haven't actually tested this in the absence of a rand-update procedure
(define (rand symbol)
  (let ((current-state random-init)) ;; random-init is a hypothetical value
    (cond ((eq? symbol 'generate)
	   (set! current-state (rand-update current-state)) ;; rand-update is a hypothetical procedure
	   current-state)
	  ((eq? symbol 'reset)
	   (lambda (new-value)
	     (set! current-state new-value)))
	  (else (error "unknown input -- RAND -- " symbol)))))
	   
