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
  (cond ((number? value) (num-equiv goal value))
	((boolean? value) (bool-equiv goal value))))

(define (test-position)
  (disp "Running tests for position procedure\n")
  (test-equivalence 0 (position 0 0 0 0))
  (test-equivalence 20 (position 0 0 20 0))
  (test-equivalence 60 (position 0 5 10 10))
  (test-equivalence 10 (position 2 2 2 2))
  (test-equivalence (/ 185 2) (position 5 5 5 5))
  (disp "\n"))

(define (test-roots)
  (disp "Running tests for root procedures\n")
  (test-equivalence 2 (root1 1 -6 8))
  (test-equivalence 4 (root2 1 -6 8))
  (test-equivalence 10.21407185181413 (root1 -9.8 100 1))
  (test-equivalence -9.990219161069284e-3 (root2 -9.8 100 1))
  (test-equivalence false (root1 3 5 6))
  (test-equivalence false (root2 3 5 6))
  (test-equivalence false (root1 3 0 1))
  (test-equivalence false (root2 3 0 1))
  (test-equivalence -3 (root1 -1 -8 -15))
  (test-equivalence -5 (root2 -1 -8 -15)))

(define (test-time-to-impact)
  (disp "Running tests for time-to-impact procedure\n")
  (test-equivalence 20.4181 (time-to-impact 100 1))
  (test-equivalence 0 (time-to-impact 0 0))
  (test-equivalence 1.4142 (time-to-impact 0 9.8))
  (test-equivalence false (time-to-impact 100 -20))
  (test-equivalence false (time-to-impact -100 20)))

(define (test-time-to-height)
  (disp "Running tests for time-to-height procedure\n")
  (test-equivalence 20.1959 (time-to-height 100 1 22))
  (test-equivalence false (time-to-height -100 1 22))
  (test-equivalence false (time-to-height 100 -1 22))
  (test-equivalence false (time-to-height 1 1 10)))

(define (test-mps-to-mph)
  (disp "Running tests for mps-to-mph procedure\n")
  (test-equivalence 101.25 (mps-to-mph 45)))

(define (test-mph-to-mps)
  (disp "Running tests for mph-to-mps procedure\n")
  (test-equivalence 44.444 (mph-to-mps 100)))

(define (test-degree-to-radian)
  (disp "Running tests for degree2radian\n")
  (test-equivalence 3.1430 (degree2radian 180))
  (test-equivalence 0 (degree2radian 0)))

(define (test-meters-to-feet)
  (disp "Running tests for meters-to-feet\n")
  (test-equivalence 3.300000000000 (meters-to-feet 1)))

(define (test-feet-to-meters)
  (disp "Running tests for feet-to-meters\n")
  (test-equivalence 1 (feet-to-meters 3.300000000)))

(define (test-hours-to-seconds)
  (disp "Running tests for hours-to-seconds\n")
  (test-equivalence 3600 (hours-to-seconds 1)))

(define (test-seconds-to-hours)
  (disp "Running tests for seconds-to-hours\n")
  (test-equivalence 1 (seconds-to-hours 3600)))

(define (test-mph-to-mps)
  (disp "Running tests for mph-to-mps\n")
  (test-equivalence 44.444444 (mph-to-mps 100.0)))

(define (test-mps-to-mph)
  (disp "Running tests for mps-to-mph\n")
  (test-equivalence 101.25 (mps-to-mph 45)))

(define (test-travel-distance-simple)
  (disp "Running tests for travel-distance-simple\n")
  (test-equivalence 22.2306 (meters-to-feet (travel-distance-simple 100 89 3)))
  (test-equivalence 668.1390 (meters-to-feet (travel-distance-simple 100 45 3)))
  (test-equivalence 63.1738 (meters-to-feet (travel-distance-simple 100 0 3))))

(define (test-find-best-angle)
  (disp "Running tests for find-best-angle\n")
  (test-equivalence 45 (find-best-angle 100 3)))

(define (test-all)
  (test-position)
  (test-roots)
  (test-time-to-impact)
  (test-time-to-height)
  (test-mps-to-mph)
  (test-mph-to-mps)
  (test-degree-to-radian)
  (test-meters-to-feet)
  (test-feet-to-meters)
  (test-hours-to-seconds)
  (test-seconds-to-hours)
  (test-mph-to-mps)
  (test-mps-to-mph)
  (test-travel-distance-simple)
  (test-find-best-angle))

(disp "\n\n")
(test-all)











