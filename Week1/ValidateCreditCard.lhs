>{-# OPTIONS_GHC -Wall #-}

Exercise 1:
-----------

First we need to find the digits of a number. Define the functions toDigits and 
toDigitsRev.

> -- turns positive integers into a list of digits
> toDigits :: Integer -> [Integer]
> toDigits n = reverse ( toDigitsRev n )

> -- turns an integer into a list of digits in reverse order
> toDigitsRev :: Integer -> [Integer]
> toDigitsRev n
>     | n <= 0 = []
>     | n < 10 = [n]
>     | otherwise  = n `mod` 10 : toDigitsRev ( n `div` 10 )

Exercise 2:
-----------

Now that we have the digits in order, we meed to double every other one starting
with the second from the right.

Examples:
doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther [1,2,3] == [1,4,3]

> -- doubles every other integer (starting from the right)
> doubleEveryOther :: [Integer] -> [Integer]
> doubleEveryOther = reverse . doubleEveryOtherReversed . reverse

> -- doubles every other integer starting from the left
> doubleEveryOtherReversed :: [Integer] -> [Integer]
> doubleEveryOtherReversed [] = []
> doubleEveryOtherReversed (x:[]) = [x]
> doubleEveryOtherReversed (x:y:rest) = x:(2*y):(doubleEveryOtherReversed rest)



