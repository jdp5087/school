###3.19###
------------------------------------------------------------
Exercise 3.19.  Redo exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.)
------------------------------------------------------------

(define (cycle? struct)
  (let ((target (cons '() '())))
    (define (iter s)
      (if (null? s)
	  false
	  (let ((next (cdr s)))
	    (if (eq? s target)
		true
		(begin (set-cdr! s target)
		       (iter next))))))
    (iter struct)))

(define w (list 'a 'b 'c 1 2 3))

(cycle? w)

(define s (list 'a 'b 'c 1 2 3))
(set-cdr! (cdr (cddddr s)) s)

(cycle? s)

;; At first I had trouble with this one, until I realized that I had misread the prompt, which had
;; specified that the data structure must be a list (which implies that the car can be ignored
;; and that only the cdr might contain a cyclic reference).

;; The idea is that if a cycle exists, then traversing the structure will ultimately end
;; up re-tracing a path that has already been traversed. By altering where the cdr points
;; to after each node is visited, we know that if we end up at target, (a pair not within
;; the original structure, then a cycle exists).

;; My solution destroys the list, unless it were modified to make a copy. Here's an implementation
;; of Floyd's algorithm, found on the sicp community wiki. I'm pretty sure this is what the authors
;; were looking for.
	
(define (contains-cycle? lst) 
  (define (safe-cdr l) 
    (if (pair? l) 
	(cdr l) 
	'())) 
  (define (iter a b) 
    (cond ((not (pair? a)) #f) 
	  ((not (pair? b)) #f) 
	  ((eq? a b) #t) 
	  ((eq? a (safe-cdr b)) #t) 
	  (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
  (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

;; Very clever indeed.