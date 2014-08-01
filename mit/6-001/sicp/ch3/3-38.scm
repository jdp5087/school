###3.38###
PROMPT:
------------------------------------------------------------
Exercise 3.38.  Suppose that Peter, Paul, and Mary share a joint bank account that initially contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary withdraws half the money in the account, by executing the following commands:
Peter: 	(set! balance (+ balance 10))
Paul: 	(set! balance (- balance 20))
Mary: 	(set! balance (- balance (/ balance 2)))

a. List all the different possible values for balance after these three transactions have been completed, assuming that the banking system forces the three processes to run sequentially in some order.

b. What are some other values that could be produced if the system allows the processes to be interleaved? Draw timing diagrams like the one in figure 3.29 to explain how these values can occur. 
------------------------------------------------------------

;;;Part a;;;
;; There are 3 unique processes that can occur, therefore we have 3! = 6 different orderings.

Peter: 	(set! balance (+ balance 10)) 110
Paul: 	(set! balance (- balance 20)) 90
Mary: 	(set! balance (- balance (/ balance 2))) 45

Peter: 	(set! balance (+ balance 10)) 110
Mary: 	(set! balance (- balance (/ balance 2))) 55
Paul: 	(set! balance (- balance 20)) 35

Mary: 	(set! balance (- balance (/ balance 2))) 50
Peter: 	(set! balance (+ balance 10)) 60
Paul: 	(set! balance (- balance 20)) 40

Mary: 	(set! balance (- balance (/ balance 2))) 50
Paul: 	(set! balance (- balance 20)) 30
Peter: 	(set! balance (+ balance 10)) 40

Paul: 	(set! balance (- balance 20)) 80
Peter: 	(set! balance (+ balance 10)) 90
Mary: 	(set! balance (- balance (/ balance 2))) 45

Paul: 	(set! balance (- balance 20)) 80
Mary: 	(set! balance (- balance (/ balance 2))) 40
Peter: 	(set! balance (+ balance 10)) 50

;;;Part b;;;
;; drawn on paper.
;; the point here is that results can become even more highly unpredictable when interleaving instructions can occur.




