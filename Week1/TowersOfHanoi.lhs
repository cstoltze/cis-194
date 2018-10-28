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
To be efficient (not sure if it is MOST efficient solution), we should:
1) move all but four discs to peg c (using all pegs as storage)
2) move three of the remaining discs to peg d (the other storage peg).
      NOTE: during this step we must use `hanoi 3 a b d` because peg c is
	        occupied by smaller discs
3) move the last disc to peg b (the target peg)
4) move the three remaining discs from peg d to peg b (`hanoi 3 d b a`)
5) move the discs from peg c to peg b (using all pegs as storage)


> -- given the number of discs and the names of the pegs return list of moves to solve
> hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
> hanoi4 n a b c d 
>     | n <= 2    = hanoi n a b d
>     | n == 3    = [(a,c),(a,d),(a,b),(d,b),(c,b)]
>     | otherwise = (hanoi4 (n-4) a c b d) ++ hanoi 3 a d b ++ [(a,b)] ++ hanoi 3 d b a ++ (hanoi4 (n-4) c b a d)


