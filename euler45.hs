pentagonal :: Integer -> Integer
pentagonal n = (n*(3*n-1) `div` 2)

pentagonalNums = [pentagonal x | x <- [1..]]

hexagonal :: Integer -> Integer
hexagonal n = n * (2*n - 1)

hexagonalNums = [hexagonal x | x <- [1..]]

recursion :: [Integer] -> [Integer] -> Integer
recursion (p:ps) (h:hs)
	| p == h    = h
	| p <  h    = recursion ps (h:hs)
	| otherwise = recursion (p:ps) hs

solution = recursion (dropWhile (<= 40755) pentagonalNums) (dropWhile (<= 40755) hexagonalNums)