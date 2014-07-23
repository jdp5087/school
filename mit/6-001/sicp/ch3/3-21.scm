###3.21###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 3.21.  Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)

``It's all wrong!'' he complains. ``The interpreter's response shows that the last item is inserted into the queue twice. And when I delete both items, the second b is still there, so the queue isn't empty, even though it's supposed to be.'' Eva Lu Ator suggests that Ben has misunderstood what is happening. ``It's not that the items are going into the queue twice,'' she explains. ``It's just that the standard Lisp printer doesn't know how to make sense of the queue representation. If you want to see the queue printed correctly, you'll have to define your own print procedure for queues.'' Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the printed results that they do. Define a procedure print-queue that takes a queue as input and prints the sequence of items in the queue.
-----------------------------------------------------------------------------------------------------------------

;; We are merely seeing the queue representation, which is a pointer to the first value in the queue, and a pointer
;; to the last value in the queue respectively.

;(define q1 (make-queue))
;; returns an empty queue which looks like ( () . () )

;(insert-queue! q1 'a)
;((a) a)
;; changes both front and rear to point to the pair ('a ()) because the list was empty
;(insert-queue! q1 'b)
;((a b) b)
;; sets the cdr of pair ('a ()) to hold the pair ('b ()), therefore making the car of the
;; representation point to (a (b ())). Also updates the cdr of the queue pair to hold ('b ())

;(delete-queue! q1)
;((b) b)
;; The front-ptr now points to (cdr (a (b ()))), which is just (b ())

;(delete-queue! q1)
;(() b)
;; Now front-ptr points to (cdr (b ())), which is (). B doesn't change, because
;; there is no need. The queue is by definition empty (see definition of empty-queue,
;; which refers to only the front-ptr to see if it is null.
;; b is only needed for inserts, and since (empty-queue?) will be true
;; if another element is added, both the car and cdr of queue will be updated.
;; for instance:
;; (insert-queue 'c)
;; would yield ((c) c)).

;; A print queue operation will simply take a queue and print the front-ptr
;; this is because this is the actual representation of the values in the list,
;; and the rear-ptr of a queue is only needed for the insertion of new values.

(define (print-queue queue)
  (front-ptr queue))


(print-queue q1)
	

