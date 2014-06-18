(define (load-lib os)
  (if (string=? os "w")
      (cd "c:/school/mit/6-001/projects/project2")
      (cd "~/Documents/school/mit/6-001/projects/project2"))
  (load "../../lib.scm"))


(load-lib "l")


###2.83###
PROMPT:
-----------------------------------------------------------------------------------------------------------------
Exercise 2.83.  Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic raise operation that will work for each type (except complex).
-----------------------------------------------------------------------------------------------------------------

;; This is getting to be such a large body of code that I started keeping it all in one place, because it simply takes too long to start up
;; and load all of the code in to the environment otherwise.

;; In this implementation, I actually created a (install-real-package) procedure that installs an implemenation of real numbers. Along with this
;; each package installs a coercion from itself to the next package up in the hierarchy.

;; The raise procedure takes an object, checks the next highest type in the hierarchy, and then looks up the coercion from the given type to
;; the next. Additionally, an error is raised if the type is not found in the coercion hierarchy, or if the type already the highest.

;; Please see coercion.scm for the code, its much better than cutting and pasting a huge amount of code every exercise.

(raise (raise (raise 1)))
;Value 61: (complex rectangular 1. . 0)

(raise (raise (raise (raise 1))))
;Error - type not raisable -- FIND-ONE-UP  complex
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): 
