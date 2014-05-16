###2.55###
PROMPT:
------------------------------------------------------------
Exercise 2.55.  Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

To her surprise, the interpreter prints back quote. Explain.
------------------------------------------------------------

(car ''abracadabra)
;; My best explanation of this is by looking at:

''abracadabra
;Value 22: (quote abracadabra)


It appears that we have quoted a symbol that would have been interpreted by the evaluator
by applying quote to abracadabra. We are seeing the data structure as the interpreter would, before evaluation.

A better way of putting this might be to say that we are seeing the symbolic representation of a symbolic representation
in the interpreter. This is confirmed by looking at:

'''abracadabra
;Value 25: (quote (quote abracadabra))



