skips :: [a] -> [[a]]
skips x = s x 1

-- 
s :: [a] -> Int -> [[a]]
s [] _ = []
s x n = (l x n) : s (tail x) (n + 1)

-- 
l :: [a] -> Int -> [a]
l [] _ = []
l x n = head x : l (drop n x) n
