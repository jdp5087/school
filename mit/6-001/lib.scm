(define disp (lambda args
   (letrec ((disp-in (lambda (arg) 
              (if (null? arg) 
                  'Done 
                  (begin 
                     (display (car arg)) 
                     (disp-in (cdr arg))))))) 
      (disp-in args))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map proc sequence)
  (if (null? sequence)
      '()
      (cons (proc (car sequence))
	    (map proc (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))






