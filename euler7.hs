-- warning: if you run solution with 10001 it will cost gigabytes of memory

-- Calculating square roots with ints
isqrt :: Int -> Int
isqrt n = ceiling (sqrt (fromIntegral n))

-- Naive method to counting primes, used for the first 200 only (which are fast to count)
naiveIsPrime :: Int -> Bool
naiveIsPrime n = length [x | x <- [2..(isqrt n)], n `mod` x == 0] == 0 

-- First two hundred primes, calculated naively
twoHundredPrimes = take 200 (2 : 3 : [x | x <- [5..], naiveIsPrime x])

-- Is the given number a prime.
-- This function is given the number to investigate
-- and previous primes to compare it against.
checkAgainst :: Int -> [Int] -> Bool
checkAgainst n []     = False
checkAgainst n (x:xs)
	| n < x * x      = False
	| n `mod` x == 0 = True
	| otherwise      = checkAgainst n xs

-- Given an Int on where to start and a list of
-- previous primes calculates the next prime.
isPrime :: Int -> [Int] -> Int
isPrime current primes
	| isIn      = isPrime (current + 2) primes
	| otherwise = current
	where isIn  = checkAgainst current primes

-- Recursively find all primes.
recursion :: Int -> Int -> [Int] -> Int
recursion cur goal primes
	| cur == goal = isPrime (last primes + 2) primes
	| cur < goal  = recursion (cur + 1) goal (primes ++ [isPrime (last primes + 2) primes])

-- Given n finds the n:th prime number.
solution :: Int -> Int
solution n
	| n < 201   = twoHundredPrimes !! (n - 1)
	| otherwise = recursion 201 n twoHundredPrimes