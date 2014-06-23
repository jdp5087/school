###3.4###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 3.4.  Modify the make-account procedure of exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.
-----------------------------------------------------------------------------------------------------------------

(define (make-account balance password)
  (let ((pass-count 0))
    (define (make-withdrawal amount)
      (if (>= balance amount)
	  (begin
	    (set! balance (- balance amount))
	    balance)
	  "Insufficient funds"))
    (define (make-deposit amount)
      (begin
	(set! balance (+ balance amount))
	balance))
    (define (call-the-cops val)
      "The fuzz have been apprised of your indescretion")
    (define (dispatch pass arg)
      (cond ((not (equal? password pass))
	     (set! pass-count (+ pass-count 1))
	     (if (>= pass-count 7)
		 call-the-cops
		 (lambda (amount) "Incorrect Password")))
	    ((equal? arg 'withdraw) make-withdrawal)
	    ((equal? arg 'deposit) make-deposit)
	    (else (error "unknown request -- MAKE-ACCOUNT -- " arg))))
    dispatch))

(define acc (make-account 100 'foo))
((acc 'foo 'withdraw) 20)
((acc 'bar 'withdraw) 20)
;Value 18: "Incorrect Password" ;; x6 tries
;Value 21: "The fuzz have been apprised of your indescretion" ;; 7th bad password




