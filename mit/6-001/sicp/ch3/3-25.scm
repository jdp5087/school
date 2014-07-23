###3.25###
PROMPT:
------------------------------------------------------------
Exercise 3.25.  Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The lookup and insert! procedures should take as input a list of keys used to access the table.
------------------------------------------------------------


(define (make-table equality-test)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
	    ((equality-test key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define (lookup-iter keys parent)
	(if (= (length keys) 1)
	    (let ((record (assoc (car keys) (cdr parent))))
	      (if record
		  (cdr record)
		  false)))
	(let ((subtable (assoc (car keys) (cdr parent))))
	  (if subtable
	      (lookup-iter (cdr keys) subtable)
	      false)))
      (lookup-iter keys local-table))
    (define (insert! . values)
      (define (create-tables values)
	(let ((key (car values)))
	  (if (= (length values) 2)
	      (cons key (cadr values))
	      (list (car values)
		    (create-tables (cdr values))))))
      (define (insert-iter values parent)
	(let ((key (car values)))
	  (cond ((< (length values) 2) (error "insert! requires at least two values " values))
		((= (length values 2)
		    (let ((record (assoc key (cdr parent))))
		      (if record
			  (set-cdr! record (cadr values))
			  (set-cdr! parent
				    (cons (cons key (cadr values))
					  (cdr parent)))))))
		(else
		 (let ((subtable (assoc key (cdr parent))))
		   (if subtable
		       (set-cdr! (cons (insert-iter (cdr values) subtable)
				       (cdr subtable)))
		       (set-cdr! parent
				 (cons (create-tables values)
				       (cdr parent)))))))))
      (insert-iter values local-table)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t3 (make-table equal?))

((t3 'insert-proc!) 'foo 'bar 'baz 'bazzam 'hello)













