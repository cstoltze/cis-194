>{-# OPTIONS_GHC -Wall #-}

Exercise 5:
-----------

return a list of moves to be performed to solve
the towers of hanoi game

Steps to solve:
1) move n-1 discs from a to c using b as temporary storage
2) move top disc from a to b
3) move n-1 discs from c to b using a as temporary storage

> type Peg = String
> type Move = (Peg, Peg)

> -- given the number of discs and the names of the pegs return list of moves to solve
> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
> hanoi 0 _ _ _ = []
> hanoi 1 a b _ = [(a,b)]
> hanoi n a b c = ( hanoi (n-1) a c b ) ++ ( hanoi 1 a b c ) ++ ( hanoi (n-1) c b a )


Exercise 6:

Similar to hanoi, but with an extra peg

Approach:
This game simplifies to hanoi when one of the extra pegs occupied.
To be fastest, we look for the shortest set of moves.
1) move all but x discs to peg c (using all pegs as storage)
2) move the remaining discs to peg b (the target peg)
      NOTE: during this step we must use `hanoi x a b d` because peg c is
	        occupied by smaller discs
3) move the discs from peg c to peg b (using all pegs as storage)

We choose the `x` that makes the shortest list of moves.

> -- given the number of discs and the names of the pegs return list of moves to solve
> hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
> hanoi4 n a b c d 
>     | n <= 2    = hanoi n a b d
>     | otherwise = shortest ( allSolutions n a b c d )

> -- gets all solutions
> allSolutions :: Integer -> Peg -> Peg -> Peg -> Peg -> [[Move]]
> allSolutions n a b c d = [hanoi4WithSplit n x a b c d | x <- [1..n]]

> -- solves hanoi4 by moving all but x discs to peg c, then uses hanoi to move the re
> -- remaining discs to peg b. Finally it moves the discs from peg c to b 
> -- using hanoi4
> hanoi4WithSplit :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
> hanoi4WithSplit n x a b c d = (hanoi4 (n-x) a c b d) ++ hanoi x a b d ++ (hanoi4 (n-x) c b a d)

> shortest :: [[Move]] -> [Move]
> shortest [] = []
> shortest [x] = x
> shortest (x:y:rest)
>   | length x > length y = shortest (y:rest)
>   | otherwise           = shortest (x:rest)
