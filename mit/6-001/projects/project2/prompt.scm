;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop-view-history0 strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (display history0))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 20 (random 21))))

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))


(define (extract-entry play associations)
  (define (get-play association)
    (car association))
  (define (get-value association)
    (cadr association))
  (define (first-association a)
    (car a))
  (define (rest-of-associations a)
    (cdr a))
  (define (first-play play)
    (car play))
  (define (second-play play)
    (cadr play))
  (define (equal-plays? possibility)
    (and (string=? (first-play play) (first-play possibility))
	 (string=? (second-play play) (second-play possibility))))
  (define (iter remaining)
    (if (equal-plays? (get-play (first-association remaining)))
	(first-association remaining)
	(iter (rest-of-associations remaining))))
  (iter associations))



(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (define (last-n-test hist n)
    (cond ((= n 0) "d")
	  ((null? hist) "c")
	  ((string=? (most-recent-play hist) "c") "c")
	  (else (last-n-test (rest-of-plays hist) (- n 1)))))
  (if (empty-history? my-history)
      "c"
      (last-n-test other-history 2)))

(define (make-eye-for-n-eyes n)
  (define (last-n-test hist n)
    (cond ((= n 0) "d")
	  ((null? hist) "c")
	  ((string=? (most-recent-play hist) "c") "c")
	  (else (last-n-test (rest-of-plays hist) (- n 1)))))
  (lambda (my-history other-history)
    (if (empty-history? my-history)
			"c"
			(last-n-test other-history n))))

(define EYE-FOR-THREE-EYES (make-eye-for-n-eyes 3))
(define EYE-FOR-FIVE-EYES (make-eye-for-n-eyes 5))

(define (make-rotating-strategy strat0 freq0 strat1 freq1)
  (define (length-of-history hist)
    (length hist))
  (define (current-procedure-iter len cur-freq cur-proc last-freq last-proc)
    (if (< (- len cur-freq) 0)
	cur-proc
	(current-procedure-iter (- len cur-freq) last-freq last-proc cur-freq cur-proc)))
  (define (current-procedure hist)
    (current-procedure-iter (length-of-history hist) freq0 strat0 freq1 strat1))
  (lambda (my-history other-history)
    ((current-procedure my-history) my-history other-history)))

(define (against-all-strategies strat strategies)
  (define (current-strategy strats) (car strats))
  (define (rest-of-strategies strats) (cdr strats))
  (define (run-round s)
    (disp "Pitting " strat " versus " s)
    (play-loop strat s)
    (disp "\n"))
  (if (null? strategies)
      (values)
      (let ((l (run-round (current-strategy strategies))))
	(against-all-strategies strat (rest-of-strategies strategies)))))

(define (make-higher-order-spastic strats)
  (define (length-of-strategies s) (length s))
  (define (length-of-history h) (length h))
  (lambda (my-history other-history)
    (let ((strat-length (length-of-strategies strats))
	  (hist-length (length-of-history my-history)))
      ((list-ref strats (remainder hist-length strat-length)) my-history other-history))))

(play-loop-view-history0 (make-higher-order-spastic all-strategies) PATSY)
      

(define all-strategies (list PATSY NASTY SPASTIC EGALITARIAN EYE-FOR-EYE EYE-FOR-TWO-EYES EYE-FOR-FIVE-EYES))

(define (run-strategies strat)
  (disp ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
  (disp ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
  (against-all-strategies strat all-strategies))  

(run-strategies (make-higher-order-spastic all-strategies))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

;(define *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))




;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
;(define (test-entry expected-values actual-values) 
;   (cond ((null? expected-values) (null? actual-values)) 
;         ((null? actual-values) #f) 
;         ((or (not (car expected-values)) 
;              (not (car actual-values)) 
;              (= (car expected-values) (car actual-values))) 
;          (test-entry (cdr expected-values) (cdr actual-values))) 
;         (else #f))) 
;
;(define (is-he-a-fool? hist0 hist1 hist2) 
;   (test-entry (list 1 1 1) 
;               (get-probability-of-c 
;                (make-history-summary hist0 hist1 hist2))))
;
;(define (could-he-be-a-fool? hist0 hist1 hist2)
;  (test-entry (list 1 1 1)
;              (map (lambda (elt) 
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)  
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0 
;                                                               hist1
;                                                               hist2)))))
