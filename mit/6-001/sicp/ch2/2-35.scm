###2.35###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>)))
-----------------------------------------------------------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
	      0
	      (map (lambda (sub-tree)
		     (if (not (pair? sub-tree))
			 1
			 (count-leaves sub-tree)))
		   t)))

(count-leaves (list (list 1 (list 2 3)) (list (list 8 9 20) 5 6)))
;Value: 8
