-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum 
       . filter even
       . takeWhile (> 1)
       . iterate (\x -> if x `mod` 2 == 0 then x `div` 2 else 3 * x + 1)


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = calculateHeight . foldr (addToShorter) Leaf

addToShorter :: a -> Tree a -> Tree a
addToShorter x Leaf = Node 0 Leaf x Leaf
addToShorter x t@(Node h l v r)
    | height l <= height r = Node h (addToShorter x l) v r
    | otherwise            = Node h l v (addToShorter x r)

calculateHeight :: Tree a -> Tree a
calculateHeight Leaf = Leaf
calculateHeight t@(Node h l v r) = Node (height t) (calculateHeight l) v (calculateHeight r)

height :: Tree a -> Integer
height Leaf = 0
height (Node h l v r) = 1 + max (height l) (height r)
    

-- Exercise 3

-- 1)
xor :: [Bool] -> Bool
xor = foldr (/=) False

-- 2)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- 3)
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (rev f) base (reverse xs)

rev f x y = f y x


-- Exercise 4
-- My summary of Seive of Sundaram
-- 1) start with list of integers [1..n]
-- 2) remove all numbers of the form i+j+2ij
--      where 1 <= i <= j
--      and i + j + 2ij <= n
-- 3) double and add 1 to the remaining numbers
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  map ((+1) . (* 2)) $ filter (`notElem` (allNumToRemove n)) [1..n]

allNumToRemove n = [z | i <- [1..n],
                        j <- [i..n],
                        let z = (i+j)+(2*i*j),
                            z <= n]
