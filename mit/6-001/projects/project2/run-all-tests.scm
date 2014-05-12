(define (run-tests os)
  (if (string=? os "w")
      (cd "c:/school/mit/6-001/projects/project2")
      (cd "~/Documents/school/mit/6-001/projects/project2"))
  (load "prompt.scm")
  (load "../../lib.scm")
  (load "test_two_player.scm"))


(run-tests "w")
