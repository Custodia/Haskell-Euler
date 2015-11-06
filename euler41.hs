import Data.List(permutations)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime a = isPrimeHelper a primes

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
	| m - 1 /= 0 && m + 1 /= 6 = False
	| p*p > a = True
	| a `mod` p == 0 = False
	| otherwise = isPrimeHelper a ps
	where m = a `mod` 6

primes = 2 : 3 : filter isPrime [5..]

-- It cannot be 9 or 8 because those are always divisible by 3 so it has to be 7.
-- This takes about 0.6 secs on my chromebook.
solution = maximum . filter isPrime . map read $ permutations "1234567"