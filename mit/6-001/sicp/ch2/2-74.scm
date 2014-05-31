###2.74###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.74.  Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to division) that contains information keyed under identifiers such as address and salary. In particular:

a.  Implement for headquarters a get-record procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type information must be supplied?

b.  Implement for headquarters a get-salary procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work?

c.  Implement for headquarters a find-employee-record procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the divisions' files.

d.  When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?
-----------------------------------------------------------------------------------------------------------------

(a)

(define (get-record employee division)
  ((get 'record division) employee (get 'file division))

;; Each division must supply an install package analogous to the install packages described in this section. The package will define
;; a record procedure that takes two parameters -- employee and file. This procedure must be installed using put, with the function tagged as 'record
;; and '{division name} for the type. Of course, the data must also be supplied, and assuming its a file that will fit in core, it could be installed
;; in the same manner, tagging the put procedure as (put 'file {division name} file).

(b)

(define (get-salary record division)
  ((get 'salary division) record))

;; This one is really straightforward. It doesn't matter how the record is structured, because each division will install their own get-salary
;; record using (put 'salary {division name} procedure)

(c)

(define (find-employee-record employee divisions)
  (if (null? divisions)
      false
      (let ((record (get-record employee (car divisions))))
	(if record
	    record
	    (find-employee-record employee (cdr divisions))))))

;;; This procedure makes the assumption that each division's get-record procedure will return false if no employee is found.

(d)

;;; In the event of a corporate takeover, Insatiable will only need to recieve an install package from the new division. This installation
;;; will provide file, get-record, and get-salary procedures, along with whatever else is required/useful from each division.

###2.75###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.75.  Implement the constructor make-from-mag-ang in message-passing style. This procedure should be analogous to the make-from-real-imag procedure given above.
-----------------------------------------------------------------------------------------------------------------

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
	   (* m (cos a)))
	  ((eq? op 'imag-part)
	   (* m (sin a)))
	  ((eq? op 'magnitude) m)
	  ((eq? op 'angle) a)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  
  dispatch)

  

