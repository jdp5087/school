###2.14###
PROMPT:
------------------------------------------------------------
Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see exercise 2.12).
------------------------------------------------------------

(let ((a (make-center-percent 30 0.01))
      (b (make-center-percent 20 0.01))
      (one (make-interval 1 1)))
  (display "center :")
  (display (center (mul-interval a b)))
  (newline)
  (display "percent:")
  (display (percent (mul-interval a b)))
  (newline)
  (display "center :")
  (display (center (add-interval a b)))
  (newline)
  (display "percent:")
  (display (percent (add-interval a b)))
  (newline)
  (display "center :")
  (display (center (add-interval (div-interval one a) (div-interval one b))))
  (newline)
  (display "percent:")
  (display (percent (add-interval (div-interval one a) (div-interval one b))))
  (newline)
  (display "center :")
  (display (center (par1 a b)))
  (newline)
  (display "percent:")
  (display (percent (par1 a b)))
  (newline)  (display "center :")
  (display (center (par2 a b)))
  (newline)
  (display "percent:")
  (display (percent (par2 a b)))
  (newline)
  (let ((d (add-interval a b))
	(c (mul-interval a b)))
    (let ((e (make-interval (/ 1 (upper-bound d)) (/ 1 (lower-bound d)))))
    (display "center :")
    (display (center (make-interval (/ 1 (upper-bound d)) (/ 1 (lower-bound d)))))
    (newline)
    (display "percent:")
    (display (percent (make-interval (/ 1 (upper-bound d)) (/ 1 (lower-bound d)))))    
    (newline)
    (display "center :")
    (display (center (mul-interval c e)))
    (newline)
    (display "percent:")
    (display (percent (mul-interval c e)))
    (newline)
    (display "center :")
    (newline)
    (display "percent:")
    (newline)
    (display "center :")
    (newline)
    (display "percent:")
    (newline)))))

It appears as though every time an interval is multiplied by another interval that is not one, the percentage resistance increases. This is the case even when an
interval is multiplied by an inverse. 

center :600.000006
percent:.01999999980000076
;;; notice here that (mul-interval a b) results in an increase in the percent tolerance
center :50.
percent:9.999999999990905e-3
center :.08333333416666668
percent:1.0000000000002236e-2
;;; while dividing the interval one by any resistance leaves the percentage intact
center :12.000000480000004
percent:.02999999919999527
center :12.
percent:1.0000000000006299e-2
center :.0200000002
percent:9.999999999990789e-3
;;; This is the inverse of (add-interval a b), which has a percentage that reflects the original percentage
center :12.000000480000004
percent:.02999999919999527
;;; however, multiplying the results of (mul-interval a b) and the inverse of (add-interval a b) increases the percentage tolerance yet again.

I dare to speculate that rounding errors have something to do with it. In the first formula, we multiply the two intervals and then divide by the sum of the same
intervals. This leads to the numbers going through significant alterations in its value, and then dividing a relatively large value by a very small one (inverse).
Small rounding errors will result in a significant loss of fidelity in the end result.

In the second formula, the values are relatively resililient, becuase dividing the "one" interval by our values mantains fidelity to our end value throughout the
calculation (the percentage resistance is not changed just to revert back to its original value again).

