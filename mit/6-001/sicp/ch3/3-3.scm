###3.3###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 3.3.  Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in

(define acc (make-account 100 'secret-password))

The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

((acc 'secret-password 'withdraw) 40)
60

((acc 'some-other-password 'deposit) 50)
"Incorrect password"
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
      (set! balance (+ balance amount)))
    (define (check-password pass-input)
      (if (equal? password pass-input)
	  true
	  false))
    (define (dispatch arg pass)
      (cond ((not (check-password pass)) "Incorrect password")
	    ((equal? arg 'withdrawal) make-withdrawal)
	    ((equal? arg 'deposit) make-deposit)
	    (else (error "unknown request -- MAKE-ACCOUNT -- " arg))))
    dispatch))

(define acc (make-account 100 'foo))

((acc 'bar 'withdrawal) 40)





;;; forget check-password, dispatch should check password with given args, and then call either withdraw or deposit

;;; Look up difference between withdraw and withdrawal













(define (make-account balance password)
  (let ((pass-count 0))
    (define (make-withdrawal amount)
      (if (>= balance amount)
	  (begin
	    (set! balance (- balance amount))
	    balance)
	  "Insufficient funds"))
    (define (make-deposit amount)
      (set! balance (+ balance amount)))
    (define (check-password pass-input)
      (if (equal? password pass-input)
	  (begin
	    (set! pass-count 0)
	    true)
	  (begin
	    (set! pass-count (+ pass-count 1))
	    (if (>= pass-count 7)
		
	    false
    (define (dispatch arg pass)
      (cond ((not (check-password


