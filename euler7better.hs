isPrime :: Int -> Bool
isPrime a = isPrimeHelper a primes

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
	| p*p > a = True
	| a `mod` p == 0 = False
	| otherwise = isPrimeHelper a ps

primes = 2 : filter isPrime [3..]

solution :: Int -> Int
solution n = primes !! n