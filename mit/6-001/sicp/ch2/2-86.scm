###2.86###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.86.  Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either ordinary numbers, rational numbers, or other numbers we might wish to add to the system. Describe and implement the changes to the system needed to accommodate this. You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.
-----------------------------------------------------------------------------------------------------------------

;; Once again, the code that I have written needed to be inserted in many different locations throughout coercions.scm.

;; I needed to work out a few bugs, because my attach tag procedure was having a hard time differentiating between
;; integers and real numbers.

;; In order to allow the real part and imag part to be integer, rational, or complex, generic procedures were needed throughout
;; the body of the install packages for complex, rectangular, and polar. This meant that each of integer, rational, and real
;; needed to have procedures added for square, sqrt, sin, cos, and atan. This wasn't particularly challenging except for
;; rational numbers. I ultimately decided to merely coerce the rational number into a real number, perform the desired operation,
;; and then drop down to an approximation of the resulting rational number. Ideal? No, but with the way I have it defined the precision
;; can be turned up arbitrarily high for the conversion back down to rational. The drop procedure was not sufficient here,
;; because we needed to be sure that the number got dropped, which it wouldn't have if the number didn't come back up the same during
;; drop. Looking back on it, I suppose I could have used project, but that is minor. Overall, the most interesting procedure that
;; came out of this exercise was math-generic-rational, which had the responsibility of raising the object to a real number,
;; performing the desired operation specified as a symbol, and then returning the number to rational.