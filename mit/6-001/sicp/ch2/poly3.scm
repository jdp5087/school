(define (first-term terms)
  (apply-generic 'first-term terms))
(define (rest-terms terms)
  (apply-generic 'rest-terms terms))
(define (empty-termlist? terms)
  (apply-generic 'empty-termlist? terms))
(define (the-empty-termlist type)
  ((get 'the-empty-termlist type)))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (adjoin-term term terms)
  ((get 'adjoin-term (type-tag terms)) term (contents terms)))
(define (max-order terms)
  (apply-generic 'max-order terms))
(define (cardinality terms)
  (apply-generic 'cardinality terms))
(define (dense-repr terms)
  (apply-generic 'dense-repr terms))
(define (sparse-repr terms)
  (apply-generic 'sparse-repr terms))
(define (pad-zeros high low)
  (make-list (- high low) 0))
(define (find-vars poly)
  (apply-generic 'find-vars poly))
(define (poly? x)
  (and (pair? x) (eq? (type-tag x) 'polynomial)))
(define (remove-duplicate-vars vars)
  (define (dup-iter seq result)
    (cond ((null? seq) result)
	  ((member (car seq) result) (dup-iter (cdr seq) result))
	  (else (dup-iter (cdr seq) (append result (list (car seq)))))))
  (dup-iter vars '()))
(define (order-vars vars)
  (sort vars (lambda (x y)
	       (let ((x-length (length (memq x (variable-hierarchy))))
		     (y-length (length (memq y (variable-hierarchy)))))
		 (> x-length y-length)))))
(define (get-ordered-variables poly)
  (order-vars (remove-duplicate-vars (find-vars poly))))
(define (expand-poly p)
  (if (poly? p)
      (apply-generic 'expand p)
      p))
(define (compress-polynomial p)
  (apply-generic 'compress-polynomial p))

(define (variable-hierarchy)
  (list 'w 'z 'y 'x))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (adjoin-termlists L1 L2)
  (cond ((empty-termlist? L1) L2)
	((empty-termlist? L2) L1)
	(else
	 (let ((t1 (first-term L1))
	       (t2 (first-term L2)))
	   (cond ((> (order t1) (order t2))
		  (adjoin-term t1
			       (adjoin-termlists (rest-terms L1)
						 L2)))
		 ((< (order t1) (order t2))
		  (adjoin-term t2
			       (adjoin-termlists L1
						 (rest-terms L2))))
		 (else (adjoin-term t1
				    (adjoin-term t2
						 (adjoin-termlists (rest-terms L1)
								   (rest-terms L2))))))))))



(define (install-polynomial-package)
     
  (define (expand p)
    (let ((terms (term-list p)))
      (let ((result (expand-termlist terms)))
	(make-poly (variable p)
		   (expand-result result)))))

  (define (expand-termlist terms)
    (if (empty-termlist? terms)
	(the-empty-termlist 'sparse)
	(let ((first (first-term terms)))
	  (if (poly? (coeff first))
	      (adjoin-term (make-term (order first) (expand-poly (coeff first)))
			   (expand-termlist (rest-terms terms)))
	      (adjoin-term first
			   (expand-termlist (rest-terms terms)))))))
  (define (expand-result terms)
    (if (empty-termlist? terms)
	(the-empty-termlist (type-tag terms))
	(adjoin-termlists (expand-term (first-term terms))
			  (expand-result (rest-terms terms)))))

  (define (expand-term term)
    (define (expand-term-iter lower-terms built-terms lower-var)
      (if (empty-termlist? lower-terms)
	  built-terms
	  (let ((first-lower (first-term lower-terms)))
	    (expand-term-iter (rest-terms lower-terms)
			      (adjoin-term (make-term (order term)
						      (tag (make-poly lower-var
								      (adjoin-term first-lower
										   (the-empty-termlist 'sparse)))))
					   built-terms)
					   lower-var))))
    (if (not (poly? (coeff term)))
	(adjoin-term term (the-empty-termlist 'sparse))
	(let ((poly-contents (contents (coeff term))))
	  (let ((lower-var (variable poly-contents))
		(lower-terms (term-list poly-contents)))
	    (expand-term-iter lower-terms (the-empty-termlist 'sparse) lower-var)))))

  (define (compress-poly p)
      (make-poly (variable p) (compress-termlist (term-list p))))

  (define (compress-termlist terms)
      (compress-terms (compress-coeffs terms)))
		 
  (define (compress-coeffs terms)
    (if (empty-termlist? terms)
	(the-empty-termlist 'dense)
	(let ((first (first-term terms)))
	  (if (poly? (coeff first))
	      (adjoin-term (make-term (order first)
				      (compress-polynomial (coeff first)))
			   (compress-coeffs (rest-terms terms)))
	      (adjoin-term first (compress-coeffs (rest-terms terms)))))))
  (define (compress-terms terms)
    (define (compress-iter first rest)
      (if (empty-termlist? terms)
	  (the-empty-termlist 'sparse)
	  (let ((result (compress-term first rest)))
	    (if (empty-termlist? (cdr result))
		(adjoin-term (car result) (cdr result))
		(adjoin-term (car result)
			     (compress-iter (first-term (cdr result))
					    (rest-terms (cdr result))))))))
    (compress-iter (first-term terms) (rest-terms terms)))

  (define (compress-term first rest)
    (define (compress-term-iter first rest viable)
      (if (empty-termlist? rest)
	  (cons first viable)
	  (let ((next (first-term rest)))
	    (if (= (order first) (order next))
		(compress-term-iter (make-term (order first)
					       (add (coeff first) ;;; IF SAME TYPE AND NOT POLY ADD, IF SAME TYPE AND POLY AND IN SAME VARS ADD, ELSE
						    (coeff next))) ;;; DON'T DO ANYTHING, ALSO MAKE SURE THAT COMPRESS IS ACTUALLY WORKING RIGHT
				    (rest-terms rest)
				    viable)
		(compress-term-iter first
				    (rest-terms rest)
				    (adjoin-term next viable))))))
    (compress-term-iter first rest (the-empty-termlist 'sparse)))
	  

  (define (seperate-expanded-poly p)
    (let ((top-var (variable p))
	  (top-terms (term-list p)))
      (define (sep-iter terms)
	(if (empty-termlist? terms)
	    '()
	    (cons (make-polynomial top-var
				   (adjoin-term (first-term terms)
						(the-empty-termlist 'sparse)))
		  (sep-iter (rest-terms terms)))))
      (sep-iter top-terms)))

  (define (pad-seperated-poly sp vars)
    (if (null? sp)
	'()
	(cons (pad-poly (car sp) vars)
	      (pad-seperated-poly (cdr sp) vars))))
  (define (pad-poly p vars)
    (let ((poly-vars (find-vars p)))
      (let ((needed-vars (remove (lambda (x)
				   (member x poly-vars))
				 vars)))
	(if (null? needed-vars)
	    p
	    (pad-poly (make-polynomial (car needed-vars)
				       (adjoin-term (make-term 0 p)
						    (the-empty-termlist 'sparse)))
		      vars)))))


  (define (in-order? v1 v2)
    (let ((v1-rank (length (memq v1 (variable-hierarchy))))
	  (v2-rank (length (memq v2 (variable-hierarchy)))))
      (< v1-rank v2-rank)))

  (define (order-seperated-poly sp vars)
    (if (null? sp)
	'()
	(cons (order-and-check (car sp) vars)
	      (order-seperated-poly (cdr sp) vars))))

  (define (order-and-check p vars)
    (define (same-order? vars1 vars2)
      (cond ((and (null? vars1) (null? vars2)) true)
	    ((null? vars1) false)
	    ((null? vars2) false)
	    ((eq? (car vars1) (car vars2)) (same-order? (cdr vars1) (cdr vars2)))
	    (else false)))
    (if (same-order? (reverse (find-vars p)) vars)
	p
	(begin
;;	  (display (find-vars p))
;;	  (newline)
;;	  (display vars)
;;	  (newline)
	  (order-and-check (order-poly p vars) vars))))

  (define (order-poly p vars)
    (let ((top-contents (contents p)))
      (let ((high-var (variable top-contents))
	    (high-termlist (term-list top-contents)))
	(let ((high-order (order (first-term high-termlist)))
	      (high-coeff (coeff (first-term high-termlist))))
	  (if (not (poly? high-coeff))
	      p
	      (let ((low-contents (contents high-coeff)))
		(let ((low-var (variable low-contents))
		      (low-termlist (term-list low-contents)))
		  (if (in-order? high-var low-var)
		      (make-polynomial high-var
				       (adjoin-term (make-term high-order
							       (order-poly high-coeff vars))
						    (the-empty-termlist 'sparse)))
		      (let ((low-order (order (first-term low-termlist)))
			    (low-coeff (coeff (first-term low-termlist))))
			(make-polynomial low-var
					 (adjoin-term (make-term low-order
								 (order-poly (make-polynomial high-var
											      (adjoin-term (make-term high-order
														      low-coeff)
													   (the-empty-termlist 'sparse)))
									     vars))
						      (the-empty-termlist 'sparse))))))))))))

  (define (get-first-term-from-poly p)
    (first-term (term-list (contents p))))

  (define (join-seperated-poly sp)
    (make-polynomial (variable (contents (car sp)))
		     (create-termlist-from-sep sp)))

  (define (create-termlist-from-sep sp)
    (if (null? sp)
	(the-empty-termlist 'sparse)
	(adjoin-term (get-first-term-from-poly (car sp))
		     (create-termlist-from-sep (cdr sp)))))

  (define (get-ordered-variables-inner poly)
    (order-vars (remove-duplicate-vars (find-v poly))))
  (define (get-longer-var-list vars1 vars2)
    (let ((len1 (length vars1))
	  (len2 (length vars2)))
      (if (> len1 len2)
	  vars1
	  vars2)))

  (define (canonical-form p vars)
    (join-seperated-poly (order-seperated-poly (pad-seperated-poly (seperate-expanded-poly (expand p)) vars) vars)))

  (define (get-canonical-forms p1 p2)
    (let ((vars1 (get-ordered-variables-inner p1))
	  (vars2 (get-ordered-variables-inner p2)))
      (let ((varlist (get-longer-var-list vars1 vars2)))
	(cons (contents (canonical-form p1 varlist))
	      (contents (canonical-form p2 varlist))))))
	     
  (define (find-v p)
    (append (list (variable p)) (find-vars (term-list p))))
  (define (poly-zero? p)
    (define (zero-iter terms)
      (if (empty-termlist? terms)
	  true
	  (let ((first (first-term terms)))
	    (if (=zero? (coeff first))
		(zero-iter (rest-terms terms))
		false))))
    (zero-iter (term-list p)))

  (define (expanded-poly? terms)
    (define (expanded-iter terms orders)
      (if (empty-termlist? terms)
	  false
	  (let ((first (first-term terms)))
	    (if (member (order first) orders)
		true
		(expanded-iter (rest-terms terms) (cons (order first) orders))))))
    (expanded-iter terms '()))
  (define (compress-poly-if-needed terms)
    (if (expanded-poly? terms)
	(compress-termlist terms)
	terms))

  (define (dense-poly? terms)
    (if (<= (max-order terms) (* (cardinality terms) 1.5))
	true
	false))
  (define (choose-repr terms)
    (let ((terms (compress-poly-if-needed terms)))
      (if (dense-poly? terms)
	  (dense-repr terms)
	  (sparse-repr terms))))
  (define (add-poly p1 p2)
    (let ((can-forms (get-canonical-forms p1 p2)))
      (let ((p1 (car can-forms))
	    (p2 (cdr can-forms)))
	(if (same-variable? (variable p1) (variable p2))
	    (let ((added-terms (add-terms (term-list p1)
					  (term-list p2))))
	      (make-poly (variable p1) added-terms)) ;;; REMOVED CHOOSE REPR HERE
	    (error "Polys not in same var -- ADD-POLY"
		   (list p1 p2))))))
  (define (mul-poly p1 p2)
    (let ((can-forms (get-canonical-forms p1 p2)))
      (let ((p1 (car can-forms))
	    (p2 (cdr can-forms)))
	(if (same-variable? (variable p1) (variable p2))
	    (let ((multiplied-terms (mul-terms (term-list p1)
					       (term-list p2))))
	      (make-poly (variable p1) multiplied-terms)) ;;; REMOVED CHOOSE REPR HERE
	    (error "Polys not in same var -- MUL-POLY"
		   (list p1 p2))))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist 'sparse)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist 'sparse)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-poly p1 p2)
    (define (same-var? seq)
      (reduce-right (lambda (x y)
		      (if (false? y)
			  false
			  (eq? x y)))
		  false
		  seq))
    (let ((variables (map variable (list p1 p2))))
      (if (not (same-var? variables))
	  (error "variables are not of the same type -- DIV-POLY " variables)
	  (let ((term-lists (map term-list (list p1 p2))))
	    (let ((div-result (apply div-terms term-lists)))
	      (list (make-poly (car variables) (choose-repr (car div-result)))
		    (make-poly (car variables) (choose-repr (cadr div-result)))))))))
  (define (div-terms L1 L2)
    (define (update-dividend L1 L2 new-term)
      (add-terms L1
		 (negate
		  (mul-term-by-all-terms new-term L2))))
					
    (if (empty-termlist? L1)
	(list (the-empty-termlist 'sparse) (the-empty-termlist 'sparse))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order T2) (order T1))
	      (list (the-empty-termlist 'sparse) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((new-term (make-term new-o new-c)))
		  (let ((rest-of-result (div-terms (update-dividend L1 L2 new-term)
						   L2)))
		    (list (add-terms (adjoin-term new-term (the-empty-termlist 'sparse))
				     (car rest-of-result))
			  (cadr rest-of-result)))))))))
		  
		  
  (define (make-poly var terms) (cons var terms))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (tag p) (attach-tag 'polynomial p))

  ;; interface to system


  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
	 (tag (add-poly p1
			(make-poly (variable p2)
				   (negate (term-list p2)))))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2)
				       (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2)
				       (let ((result (div-poly p1 p2)))
					 (list (tag (car result)) (tag (cadr result))))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (negate (term-list p))))))
  (put 'zero '(polynomial) (lambda (p) (poly-zero? p)))
  (put 'sparse-repr '(polynomial) (lambda (p) (sparse-repr (term-list p))))
  (put 'dense-repr '(polynomial) (lambda (p) (dense-repr (term-list p))))
  (put 'find-vars '(polynomial) (lambda (p) (find-v p)))
  (put 'expand '(polynomial) (lambda (p) (tag (expand p))))
  (put 'compress-polynomial '(polynomial) (lambda (p) (tag (compress-poly p))))
  'done)

(define (install-sparse-package)
  ;; Operations on sparse term-lists

  (define (find-v terms)
      (if (empty-termlist? terms)
	  (the-empty-termlist)
	  (let ((first (first-term terms)))	  
	    (cond ((poly? (coeff first)) (append (find-vars (coeff first)) (find-v (rest-terms terms))))
		  (else (find-v (rest-terms terms)))))))	  

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (length-termlist terms) (length terms))
  (define (empty-termlist? term-list) (null? term-list))
  (define (sparse-repr terms)
    terms)

  (define (dense-repr terms)
    (define (dense-iter terms prev-order)
      (cond ((and (empty-termlist? terms) (= prev-order 0))
	     (the-empty-termlist))
	    ((and (empty-termlist? terms) (> prev-order 0))
	     (cons 0 (dense-iter terms (- prev-order 1))))
	    (else
	     (let ((current-term (first-term terms))
		   (current-order (order (first-term terms))))
	       (cond ((= (- prev-order 1) current-order)
		      (cons (coeff current-term)
			    (dense-iter (rest-terms terms)
					(order current-term))))
		     (else
		      (cons 0 (dense-iter terms (- prev-order 1)))))))))
    (dense-iter terms (+ (max-order terms) 1)))

  (define (max-order terms)
    (if (empty-termlist? terms)
	0
	(order (first-term terms))))
  (define (cardinality terms) (length-termlist terms))
  (define (neg terms)
    (map (lambda (term)
	   (make-term (order term)
		      (negate (coeff term))))
	 terms))
  (define (tag terms) (attach-tag 'sparse terms))
  ;;; Interface to outside


  (put 'the-empty-termlist 'sparse (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(sparse) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(sparse) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(sparse) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(sparse) (lambda (terms)
				(tag (sparse-repr terms))))
  (put 'dense-repr '(sparse) (lambda (terms)
				 (attach-tag 'dense (dense-repr terms))))

  (put 'adjoin-term 'sparse (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(sparse) (lambda (terms) (cardinality terms)))
  (put 'max-order '(sparse) (lambda (terms) (max-order terms)))
  (put 'negate '(sparse) (lambda (p) (tag (neg p))))
  (put 'find-vars '(sparse) (lambda (terms) (find-v terms)))
  'done)

(define (install-dense-package)
  (define (find-v terms)
      (if (empty-termlist? terms)
	  (the-empty-termlist)
	  (let ((first (first-term terms)))	  
	    (cond ((poly? (coeff first)) (append (find-vars (coeff first)) (find-v (rest-terms terms))))
		  (else (find-v (rest-terms terms)))))))

  (define (zero-term? term) (= 0 (coeff term)))
  (define (max-order terms) (- (length terms) 1))
  (define (cardinality terms)
    (length (filter (lambda (x) (not (=zero? x))) terms)))
  (define (the-empty-termlist) '())
  (define (empty-termlist? terms) (null? terms))
  (define (first-term terms)
    (make-term (max-order terms) (car terms)))
  (define (add-term t1 t2)
    (make-term (order t1) (add (coeff t1) (coeff t2))))
  (define (rest-terms terms) (cdr terms))
  (define (dense-repr terms)
    terms)
  (define (order-index terms order)
    (- (max-order terms) order))
  (define (get-term-by-order terms order)
    (make-term order (list-ref terms (order-index terms order))))
  (define (adjoin-term term terms)
    (define (gen-append-list term terms max-order term-order)
      (if (> term-order max-order)
	  (extend-termlist term terms max-order term-order)
	  (insert-termlist term terms max-order term-order)))
    (define (extend-termlist t tl mo to)
      (append (list (coeff t))
	      (make-list (- to (+ mo 1)) 0)
	      tl))
    (define (insert-termlist t tl mo to)
      (append (list-head tl (- mo to))
	      (list (coeff (add-term (get-term-by-order tl to)
				     t)))
	      (list-tail tl (+ 1 (order-index tl to)))))
    (cond ((and (empty-termlist? terms) (= (order term) 0))
	   (list (coeff term)))
	  (else (gen-append-list term
				 terms
				 (max-order terms)
				 (order term)))))
  (define (sparse-repr terms)
    (cond ((empty-termlist? terms) (the-empty-termlist))
	  ((zero-term? (first-term terms)) (sparse-repr (rest-terms terms)))
	  (else (cons (first-term terms) (sparse-repr (rest-terms terms))))))
  (define (neg terms)
    (map (lambda (x) (negate x)) terms))
  (define (tag terms)
    (attach-tag 'dense terms))


  
  ;;; Interface to outside

  (put 'the-empty-termlist 'dense (lambda () (tag (the-empty-termlist))))
  (put 'empty-termlist? '(dense) (lambda (terms) (empty-termlist? terms)))
  (put 'first-term '(dense) (lambda (terms) (first-term terms)))
  (put 'rest-terms '(dense) (lambda (terms) (tag (rest-terms terms))))
  (put 'sparse-repr '(dense) (lambda (terms) (attach-tag 'sparse (sparse-repr terms))))
  (put 'dense-repr '(dense) (lambda (terms) (tag (dense-repr terms))))
  (put 'adjoin-term 'dense (lambda (term terms) (tag (adjoin-term term terms))))
  (put 'cardinality '(dense) (lambda (terms) (cardinality terms)))
  (put 'max-order '(dense) (lambda (terms) (max-order terms)))
  (put 'negate '(dense) (lambda (terms) (tag (neg terms))))
  (put 'find-vars '(dense) (lambda (terms) (find-v terms)))
  'done)

(define (install-packages)
  (install-scheme-number-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-polynomial-package)
  (install-dense-package)
  (install-sparse-package))
(install-packages)


(define test-poly1 (make-polynomial 'z (list 'sparse
				      (list 2
					    (make-polynomial 'x (list 'sparse
								(list 2
								      (make-polynomial 'y (list 'dense 3 2)))
								(list 1
								      (make-polynomial 'y (list 'dense 4 5)))))))))

(define test-poly2 (make-polynomial 'z (list 'sparse
				      (list 2
					    (make-polynomial 'x (list 'sparse (list 20 4) (list 13 5)))))))
							

test-poly1
test-poly2

(define simple-poly (make-polynomial 'x (list 'sparse (list 2 3) (list 1 4))))

(begin
  (install-packages)
  (add (expand-poly simple-poly)
       (expand-poly simple-poly)))

  
(define (view-terms poly)
  (define (view-iter terms)
    (if (empty-termlist? terms)
	'()
	(begin
	  (display (first-term terms))
	  (newline)
	  (view-iter (rest-terms terms)))))
	  
  (let ((terms (cdr (contents poly))))
    (view-iter terms)))
    
;;; BUGS:

;;; It doesn't look like theres a tag in dense termlists during addition, yet the procedure still works

;;; Pad doesn't appear to be working
;;; Program should not try to add when coeffs are not in same type. meaning you cant combine x and 2xyz or whatever
;;; Also check your math, so you know what the final result should look like.

;;;Rhythmbox

















