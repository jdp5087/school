###2.42###
PROMPT:
------------------------------------------------------------
Exercise 2.42.  

The ``eight-queens puzzle'' asks how to place eight queens on a chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal). One possible solution is shown in figure 2.8. One way to solve the puzzle is to work across the board, placing a queen in each column. Once we have placed k - 1 queens, we must place the kth queen in a position where it does not check any of the queens already on the board. We can formulate this approach recursively: Assume that we have already generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board. For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column. Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens. This produces the sequence of all ways to place k queens in the first k columns. By continuing this process, we will produce not only one solution, but all solutions to the puzzle.

We implement this solution as a procedure queens, which returns a sequence of all solutions to the problem of placing n queens on an n× n chessboard. Queens has an internal procedure queen-cols that returns the sequence of all ways to place queens in the first k columns of the board.

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and new-row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other.)
------------------------------------------------------------
(define (flatmap proc sequence)
  (accumulate append () (map proc sequence)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)

  (define (get-n n sequence)
    (if (= 1 n)
	(car sequence)
	(get-n (- n 1) (cdr sequence))))
  
  (define (remove-value val sequence)
    (accumulate (lambda (x y)
		  (if (equal? x val)
		      y
		      (cons x y)))
		()
		sequence))
  
  (define empty-board ())

  (define (adjoin-position row col positions)
    (append positions (list (list row col))))

  (define (safe? k solution)
    (define (abs-dif taken new)
	     (abs (- taken new)))
    (define (safe-iter val sol)
      (if (null? sol)
	  true
	  (let ((taken-row (get-n 1 (car sol)))
		(taken-col (get-n 2 (car sol)))
		(new-row (get-n 1 val))
		(new-col (get-n 2 val)))
	    (if (or (= taken-row new-row)
		    (= (abs-dif taken-row new-row) (abs-dif taken-col new-col)))
		false
		(safe-iter val (cdr sol))))))
    (let ((new (get-n k solution)))
      (let ((rest (remove-value new solution)))
	(safe-iter new rest))))

  (define (test-pred k positions)
    (newline)
    (display positions)
    true)

  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))

  (queen-cols board-size))

;;; The output to queens 8 is so long I'll just show the output for queens 4.
;;; One very important note here is that a strong understanding of the algorithm is required before
;;; diving in to write the code. Working with higher order procedures creates a lot of moving parts
;;; that become extremely complicated to think through procedurally. I made a logic error in my implementation
;;; of adjoin-position and as a result spent a whole day trying to debug my mistake. I had to step back
;;; and think from start to finish through the algorithm before I could develop a satisfactory implementation.
;;; if it feels like the design is overly complex, it's probably because an error was made somewhere.

(queens 4)
;Value 39: (((2 1) (4 2) (1 3) (3 4)) ((3 1) (1 2) (4 3) (2 4)))


