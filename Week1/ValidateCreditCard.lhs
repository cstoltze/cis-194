>{-# OPTIONS_GHC -Wall #-}

First we need to find the digits of a number. Define the functions

> -- turns positive integers into a list of digits
> toDigits :: Integer -> [Integer]
> toDigits n = reverse ( toDigitsRev n )

> -- turns an integer into a list of digits in reverse order
> toDigitsRev :: Integer -> [Integer]
> toDigitsRev n
>     | n <= 0 = []
>     | n < 10 = [n]
>     | otherwise  = n `mod` 10 : toDigitsRev ( n `div` 10 )

