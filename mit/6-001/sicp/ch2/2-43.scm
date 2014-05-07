###2.43###
------------------------------------------------------------
Exercise 2.43.  Louis Reasoner is having a terrible time doing exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6× 6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

Explain why this interchange makes the program run slowly. Estimate how long it will take Louis's program to solve the eight-queens puzzle, assuming that the program in exercise 2.42 solves the puzzle in time T.
------------------------------------------------------------

The problem here is that there is a long recursive call placed in the inner loop. Each call other than the top level
(queen calls 8) will result in 8 calls to every step below it. This means that (queens 8) will result in 8 calls to
(queens 7) and 8 calls to (queens 6) for each of those 8 calls. This goes on until (queens 1) and ultimately (queens 0)
will be called 8 times for each call to (queens 1). Therefore, the running time of this algorithm, if the other version
ran in time T. will run in T^8 because the complexity grows by T for each recursive call to the step below.






