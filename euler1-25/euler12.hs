-- Sqrt with ints
isqrt :: Int -> Int
isqrt n = ceiling (sqrt (fromIntegral n))

-- How many dividers does the number have.
divs :: Int -> Int
divs n = 2 * length [x | x <- [1..(isqrt n)], n `mod` x == 0]

-- Recursion to find a triangular value with more than 500 dividers.
recursion :: Int -> Int -> Int
recursion last n
    | divsr > 500 = cur
    | otherwise = recursion cur (n + 1)
    where cur   = last + n
          divsr = divs cur

-- The solution
solution = recursion 0 1