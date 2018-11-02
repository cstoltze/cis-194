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

-- Exercise 2 Local maxima

-- localMaxima returns a list of local maxima from a list of integers
-- p is an alias for localMaxima (because of code Golf)
-- if there are three or more elements, we check if the middle element is a 
-- local maximum. If so, we put it in a list with the rest of the localMaxima
-- (found by applying localMaxima to the remainder of the list). Otherwise, we
-- just find the localMaxima in the rest of the list.
-- If there are fewer than three elements left, there can no longer be any
-- localMaxima, so we return [] in that case
-- CodeGolf -- if y is a maximum, then z isn't -- no need to check, we can skip
-- ahead by just doing p (z:r)
-- CodeGolf -- True has fewer letters than otherwise
localMaxima :: [Integer] -> [Integer]
localMaxima = p
p (x:y:z:r)
   | y > x && y > z  = y : p (z:r)
   | True            = p (y:z:r)
p _ = []
