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


Exercise 3:
-----------

Define the function sumDigits to sum the digits in a list of integers

> -- sums the digits in a list of (possibly multi-digit) integers
> sumDigits :: [Integer] -> Integer
> sumDigits xs = sum ( map sumDigitsInteger xs )

> -- sums the digits of an integer
> sumDigitsInteger :: Integer -> Integer
> sumDigitsInteger = sum . toDigitsRev


Exercise 4:
-----------

Define the function to validate if a credit card number could be vaild

> -- Indicates if a number could be a valid credit card number

> validate :: Integer -> Bool
> validate = divisibleByTen . sumDigits . doubleEveryOther . toDigits

> -- is the number divisible by ten
> divisibleByTen :: Integer -> Bool
> divisibleByTen x = 0 == x `mod` 10
