(define (test-equivalence goal value)
  (define (num-equiv goal value)
    (define tolerance 0.001)
    (if (< (abs (- goal value)) tolerance)
	(display "Test passed\n")
	(disp value " is not within " tolerance " of " goal "\n")))
  (define (bool-equiv goal value)
    (if (eqv? goal value)
	(display "Test passed\n")
	(disp value " is not equivalent to " goal "\n")))
  (define (pair-equiv goal value)
    (if (and (< (abs (- (car value) (car goal)))) (< (abs (- (cdr value) (cdr goal)))))
	(display "Test passed\n")
	(disp value " is not equivalent to " goal "\n")))
  (cond ((number? value) (num-equiv goal value))
	((boolean? value) (bool-equiv goal value))
	((pair? value) (pair-equiv goal value))))











(define (run-game os)
  (if (string=? os "w")
      (cd "c:/school/mit/6-001/projects/project2")
      (cd "~/Documents/school/mit/6-001/projects/project2"))
  (load "../../lib.scm")
  (load "prompt.scm"))


(run-game "l")
