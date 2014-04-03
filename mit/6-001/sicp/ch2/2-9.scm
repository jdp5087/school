PROMPT:
------------------------------------------------------------
Exercise 2.9.  The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.
------------------------------------------------------------

consider an interval i1 which has upper bound h1 and lower bound l1, and also consider an interval i2 which has upper bound h2 and lower bound l2:

Using the definition of width, we can combine the intervals through addition, and then find their width.

width = (high - low)/2

total_added_width = ((h1+h2)-(l1+l2))/2

Or, we find that we can find the width of i1 and i2, add their widths, and we have the same result:

width1 = (h1-l1)/2
width2 = (h2-l2)/2
combined_added_width = (h1-l1)/2 + (h2-l2)/2 = (h1-l1+h2-l2)/2 = ((h1+h2)-(l1+l2))/2

Now, we can also confirm the same phenomenon with subtraction.

total_subtracted_width = ((h1-h2)-(l1-l2))/2

Now we find each width seperately and combine:

width1 = (h1-l1)/2
width2 = (h2-l2)/2
combinded_subracted_width = (h1-l1)/2 - (h2-l2)/2 = ((h1-l1)-(h2-l2))/2 = (h1-l1-h2+l2)/2 = ((h1-h2)-(l1-l2))/2

so, again, we see that the results are the same.


Ok, so lets consider multiplication of widths by example.

consider the interval (-1,1) and (-2,3)

When we multiply these intervals, we get (-3, 3), which would give a width of 3. This is not the same as multiplying the widths of the original intervals,
which is 1*2.5=2.5

Furthermore, the same intervals demonstrate that we cannot derive the widths through division of the intervals.

The process that divides intervals multiplies x times the reciprocal of y.

So first we take the reciprocal fo y (with upper and lower bound switched), which is (1/3,-1/2).

Then we simply have the multiplication process of (-1,1)*(1/3,-1/2), which results in (-1/2,1/2), and a width of 1/2.
We cannot derive this same width by dividing the widths of x and y, (1/2.5), which results in 2/5.
