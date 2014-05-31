###2.76###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.76.  As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies -- generic operations with explicit dispatch, data-directed style, and message-passing-style -- describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?
-----------------------------------------------------------------------------------------------------------------

;; The generic operations with explicit dispatch procedure will need require changes to every generic procedure that has a typed procedure added.
;; For instance, adding a third hypothetical representation to the imaginary numbers example would require that real-part, imag-part,
;; magnitude, and angle have an extra clause added to check the tag of the data being checked.

;; The data-directed style will require that a put statement for every new procedure. This is relatively straightforward, and the addition will
;; simply call (put 'operation 'data-type procedure)

;; The message-passing style is perhaps the easiest to introduce to a large system, because it every procedure only acts on itself, therefore
;; the object doesn't need to identify itself to the rest of the system, and only requires that the procedure being called is defined internally
;; which it will be if the operation is generic and in use within the system.

;; The organization most appropriate for systems where types must be added frequently would be the message passing system, because the type is
;; defined internally, and the rest of the system does not require alteration to continue functioning. As pointed out, the downfall of this style
;; is that it permits only generic procedures of one argument. Thus if more flexibility in parameters were required, data-driven programming would
;; be preferable.

;; In a system where procedures must be added frequently, it seems to be a little bit less convenient for all of the styles in question.
;; In an explicit dispatch style, this will require that a generic procedure be written, with procedures for each specific type. The message-passing
;; style will require a new clause be added to every constructor for the desired procedure, and the data-directed style will require a new procedure
;; be added to each install package and be added to the lookup table with put. I would personally choose the data-directed style because it seems
;; to be a good compromise of flexibility and expressive ability (number of parameters) between the styles discussed here.

