-- True if given number is a prime, else false.
isPrime :: Int -> Bool
isPrime a = isPrimeHelper a primes

-- Recursively check by division against previous primes.
isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
	| m - 1 /= 0 && m + 1 /= 6 = False
	| p*p > a = True
	| a `mod` p == 0 = False
	| otherwise = isPrimeHelper a ps
	where m = a `mod` 6

-- List of all primes.
primes = 2 : 3 : filter isPrime [5..]

-- Gets the sum of the primes up to given number.
solution :: Int -> Int
solution n = sum (takeWhile (< n) primes)