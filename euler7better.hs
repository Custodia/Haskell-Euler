isPrime :: Int -> Bool
isPrime a = isPrimeHelper a primes

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
	| m - 1 /= 0 && m + 1 /= 6 = False
	| p*p > a = True
	| a `mod` p == 0 = False
	| otherwise = isPrimeHelper a ps
	where m = a `mod` 6

primes = 2 : 3 : filter isPrime [5..]

solution :: Int -> Int
solution n = primes !! (n - 1)