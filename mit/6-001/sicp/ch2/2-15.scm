###2.15###
PROMPT:
------------------------------------------------------------
Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa's system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a ``better'' program for parallel resistances than par1. Is she right? Why?
------------------------------------------------------------

My answer to the previous question showed that indeed, par2 leads to tigther error bounds than par1. The reason for this is because when we calculate par1, we are using the formula (R1R2/R1+R2). This leads to two uncertain intermediate variables (a product and a sum) which are then divided. While addition does not change the percentage tolerance of an interval, multiplication does. Therefore, multiplication of two uncertain intervals increases the uncertainty. So, we are multiplying by two uncertain variables twice in this calculation, because division is just multiplication by an inverse.

In par2, division is used three times. However, this calculation is slightly different, because we are dividing an interval with no uncertainty (because it is an interval that ranges between 1 and 1). Thus, multiplication of an an interval without uncertainty by the inverse of an interval with uncertainty will not increase the percentage tolerance. In other words, the percentage tolerance will be the same as the inverse of the uncertain interval. This means that for calculating tighter bounds, Eva Lu Ator is correct.

