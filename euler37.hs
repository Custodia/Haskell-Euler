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

-- All primes under a million
primes = 2 : 3 : filter isPrime [5..]

truncatable :: Int -> Bool
truncatable num
	| num < 10    = False
	| otherwise   = truncatableHelperL (show num) && truncatableHelperR (show num)

truncatableHelperL :: String -> Bool
truncatableHelperL [] = True
truncatableHelperL str
	| isPrime (read str) = truncatableHelperL (drop 1 str)
	| otherwise = False

truncatableHelperR :: String -> Bool
truncatableHelperR [] = True
truncatableHelperR str
	| isPrime (read str) = truncatableHelperR (take (length str - 1) str)
	| otherwise = False

truncatablePrimes = filter truncatable primes

solution = sum $ take 11 truncatablePrimes