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
  (define (dispatch pass arg)
    (cond ((not (equal? password pass)) (lambda (amount) "Incorrect Password"))
	  ((equal? arg 'withdraw) make-withdrawal)
	  ((equal? arg 'deposit) make-deposit)
	  (else (error "unknown request -- MAKE-ACCOUNT -- " arg))))
  dispatch)

(define acc (make-account 100 'foo))

((acc 'bar 'withdraw) 40)
;Value 13: "Incorrect Password"
((acc 'foo 'withdraw) 40)
;Value: 60
((acc 'foo 'deposit) 20)
;Value: 80





