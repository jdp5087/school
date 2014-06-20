###2.90##
-----------------------------------------------------------------------------------------------------------------
Exercise 2.90.  Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.
-----------------------------------------------------------------------------------------------------------------

The authors were not exaggerating when they said that this would be a major effort. I tried to write the new structure from bottom up and failed.
It took a lot of thought to finally arrive at a well-structured solution. I learned that in many cases top-down design is much more successful,
because it allowed me to ignore many of the details while I planned out a cohesive design. Please see poly2.scm for my final solution (note that
coercion-2-86.scm must be run first in order for all dependencies in poly2.scm to work.
