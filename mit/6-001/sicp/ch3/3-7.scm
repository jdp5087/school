###3.7###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 3.7.  Consider the bank account objects created by make-account, with the password modification described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.
-----------------------------------------------------------------------------------------------------------------
(define (make-account balance password)
  (let ((passwords (list password)))
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
      (cond ((not (member pass passwords)) (lambda (amount) "Incorrect Password"))
	    ((equal? arg 'withdraw) make-withdrawal)
	    ((equal? arg 'deposit) make-deposit)
	    ((equal? arg 'new-acc)
	     (lambda (new-pass)
	       (set! passwords (cons new-pass passwords))
	       dispatch))
	    (else (error "unknown request -- MAKE-ACCOUNT -- " arg))))
    dispatch))

(define (make-joint acc current-pass add-pass)
  (let ((new-acc ((acc current-pass 'new-acc) add-pass)))
    (if (not (procedure? new-acc))
	(error "Password was invalid -- MAKE-JOINT")
	new-acc)))




(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 30)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;Value: 70
((paul-acc 'rosebud 'deposit) 40)
;Value: 110
((paul-acc 'foo 'withdrawal) 10)
;Value 12: "Incorrect Password"
(define mary-acc (make-joint paul-acc 'foo 'bar))
;Password was invalid -- MAKE-JOINT


;; Note that the procedure raises an error if an attempt is made to make a joint account with an invalid password

;; This is because simply returning a string does not make it obvious that the account creation failed, since the result
;; is merely saved with a define statement.




