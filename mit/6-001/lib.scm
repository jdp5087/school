(define disp (lambda args
   (letrec ((disp-in (lambda (arg) 
              (if (null? arg) 
                  'Done 
                  (begin 
                     (display (car arg)) 
                     (disp-in (cdr arg))))))) 
      (disp-in args))))