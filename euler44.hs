pentagonal :: Float -> Int
pentagonal n = round ((n*(3*n-1))/2)

pentagonalNums = [pentagonal x | x <- [1..]]

isPentagonal :: Int -> Bool
isPentagonal n = n == (head $ dropWhile (<n) pentagonalNums)

recursion :: Int -> Int -> Int
recursion j k
	| j == k = recursion 1 (k+1)
    | isPentagonal minus && isPentagonal plus = minus
	| otherwise = recursion (j + 1) k
	where pentJ = pentagonalNums !! (j - 1)
	      pentK = pentagonalNums !! (k - 1)
	      minus = pentK - pentJ
	      plus  = pentJ + pentK