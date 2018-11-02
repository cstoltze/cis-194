skips :: [a] -> [[a]]
skips x = s x 1

-- gets a list of sublists (created by function l). The first list has no skips
-- and starts from the first element. The next list starts on the second
-- element and has one more skip... Each list sent to l starts one element
-- later and has one more skip that the previous call to l
s :: [a] -> Int -> [[a]]
s [] _ = []
s x n = (l x n) : s (tail x) (n + 1)

-- gets a sublist of x, starting with the first element, and skipping n - 1
-- elements between 
l :: [a] -> Int -> [a]
l [] _ = []
l x n = head x : l (drop n x) n
