PROMPT:
------------------------------------------------------------
Exercise 2.11.  In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications." Rewrite this procedure using Ben's suggestion.
------------------------------------------------------------

There are 16 possible permutations of the signs of the numbers:

++ ++
++ +-
++ --
+- --
+- -+
+- ++
+- +-
++ -+
-+ ++
-+ +-
-+ --
-- --
-- -+
-- ++
-- +-
-+ -+

Of these, only 9 are possible, because 7 have an interval where the upper-bound is less than the lower-bound. Let's explore the multiplications that we need to make.

++ ++ (1,3) (2,4)X
++ -- (2,3) (1,4)X
++ -+ (2,3) (2,4)X
-+ ++ (1,4) (2,4)X
-+ -- (2,3) (1,3)X
-- -- (2,4) (1,3)X
-- -+ (1,4) (1,3)X
-- ++ (1,4) (2,3)X
-+ -+ (1,4)/(2,3) (1,3)/(2,4)X

(the X indicates that I tested each case with the new implementation)

Now we have an idea of how to impliment mul-interval:

(define (mul-interval x y)
  (let ((a (lower-bound x))
	(b (upper-bound x))
	(c (lower-bound y))
	(d (upper-bound y)))
    (define (test-mul w x y z comp)
      (if (comp (* w x) (* y z))
	  (* w x)
	  (* y z)))
    (define (interval-by-index w x y z)
      (make-interval (* w x)
		     (* y z)))
    (if (> a 0)
	(if (> c 0)
	    (interval-by-index a c b d) ;;; ++++
	    (if (> d 0)
		(interval-by-index b c b d) ;;; ++-+
		(interval-by-index b c a d))) ;;; ++--
	(if (> b 0)
	    (if (> c 0)
		(interval-by-index a d b d) ;;; -+++
		(if (> d 0)
		    (make-interval (test-mul a d b c <)
				   (test-mul a c b d >)) ;;; -+-+
		    (interval-by-index b c a c))) ;;; -+--
	    (if (> c 0)
		(interval-by-index a d b c) ;;; --++
		(if (> d 0)
		    (interval-by-index a d a c) ;;; ---+
		    (interval-by-index b d a c))))))) ;;; ----

First, I saved each low and high value for both intervals as a b c d. This will reduce the number of times we need to access the selectors,
 and makes the code more readable. Additionally, The nested if structure may appear to be unneccessarily complicated. However, using a cond statement would
require 8 comparisons in the worst case. This implementation only requires 4 in the worst case, in the best case only 2.
 