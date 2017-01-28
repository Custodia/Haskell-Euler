-- Solves the largest divisor of a number.
largestDivisor :: Int -> Int -> Int -> Int
largestDivisor n div max
  | div > h = max
  | n `mod` div == 0 = largestDivisor (n `quot` bigDiv) bigDiv maxima
  | otherwise = largestDivisor n (div + 1) max
  where h = (n `quot` 2)
        bigDiv = maximum [div, n `quot` div]
        maxima = maximum [max, bigDiv]

-- Recursively counts counts divisors until none are found
divisorRecursion :: Int -> Int
divisorRecursion n
  | n < 0 	 = error "Please give a non-negative number."
  | result > 0 = divisorRecursion result
  | otherwise  = n
  where result = largestDivisor n 2 0

main = do
  let result = show (divisorRecursion 600851475143)
  putStrLn result
  return 0
