fibonacci :: Int -> [Int]
fibonacci n
	| n == 0 = [1]
	| n == 1 = [1, 1]
	| otherwise = previous ++ [((previous !! (n - 1)) + (previous !! (n - 2)))]
	where previous = fibonacci (n - 1)

-- sum these for the correct result.
evenFibonacci = [x | x <- fibonacci 32, x `mod` 2 == 0]