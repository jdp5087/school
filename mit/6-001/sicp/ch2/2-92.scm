###2.92###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.92.  By imposing an ordering on variables, extend the polynomial package so that addition and multiplication of polynomials works for polynomials in different variables. (This is not easy!)
-----------------------------------------------------------------------------------------------------------------

;; I wrote a great deal of code in order to solve this problem. I won't put the code in here. Please see poly3.scm for the actual code.

;; The general idea was to make a main dispatching procedure, which was called within add-poly and mul poly.
;; This procedure, get-canonical-forms found the union of the list of varibles in both polynomials, and ordered this list according to a
;; hierarchy. Then, the polynomials were expanded, so that each term in the top-level polynomial contained only 1 term in its termlist, and
;; so on recursively down each polynomial's coefficient. This expansion made the rest of the process much more straightforward, because
;; as a result it was much easier to reason about how to go about ordering variables according to a hierarchy. The next step was to seperate
;; each term in the top-level polynomial, so that we now have a list of one term polynomials, with a linear recursive structure. Each term can then
;; be padded so that if the union of variables in our two original polynomials was 'x 'y and 'z, an original polynomial 'x would be
;; padded so that it has an implicit 'y and 'z, which can be represented as a termlist with one term, 'y to the 0 with a coefficient of 1,
;; for example. After padding, the terms could then be ordered according the the hierarchy specified by the variables extracted at the beginning
;; of the dispatch from get-canonical-forms. The only step remaining was to join the seperated polynomials into one top level polynomial for the
;; highest variable in our hierarchy.

;; I was also working on a compress-polynomial procedure, but there are complications with that, and I decided that I'll get back to this. However,
;; this part is only gravy because we can add arbitrary polynomials without, as long as the variables in our two added polynomials are specified
;; in the variable hierarchy! Great success!