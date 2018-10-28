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

