sumOfSquares :: Int -> Int
sumOfSquares n = sum [x * x | x <- [1..n]]

squareOfSums :: Int -> Int
squareOfSums n = (sum [1..n]) ^ 2

sumSquareDifference :: Int -> Int
sumSquareDifference n = squareOfSums n - sumOfSquares n