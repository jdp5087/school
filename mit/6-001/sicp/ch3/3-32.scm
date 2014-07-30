###3.32###
PROMPT:
------------------------------------------------------------
Exercise 3.32.  The procedures to be run during each time segment of the agenda are kept in a queue. Thus, the procedures for each segment are called in the order in which they were added to the agenda (first in, first out). Explain why this order must be used. In particular, trace the behavior of an and-gate whose inputs change from 0,1 to 1,0 in the same segment and say how the behavior would differ if we stored a segment's procedures in an ordinary list, adding and removing procedures only at the front (last in, first out). 
------------------------------------------------------------

;; The reason that the order of actions in the agenda are important can be illustruted by the example of the and gate provided above.

;; When the input of the and gate changed from 0, 1 to 1, 0, each change in signal on in-1 and in-2 trigger an action. Thus, as in-1 changes to 1, the and-action-procedure is
;; called, which calculates a logical-and of 1, 1, which results in changing the output wire to 1. The second action will be another and-action procedure, which will be
;; triggered when in-2 changes to 0, which calculates a logical and of 1, 0, and adds that to the same time-segment in the-agenda. With a queue data structure holding these
;; actions, there is no problem. However, if we had a last-in first-out data structure, we would then set the value of the wire to 0, and then set the value to 1, where it would
;; stay, and we would end up with an erroneous value on the output from the and-gate in question.
