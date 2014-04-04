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

++ ++ (1,3) (2,4)
++ -- (2,3) (2,3)
++ -+ (2,3) (2,4)
-+ ++ (1,4) (2,4)
-+ -- (2,3) (1,3)
-- -- (2,4) (1,3)
-- -+ (1,4) (1,3)
-- ++ (1,4) (2,3)
-+ -+ (1,4)/(2,3) (1,3)/(2,4)



Now we have an idea of how to impliment mul-interval:

(define (mul-interval x y)
  (cond ((and 

