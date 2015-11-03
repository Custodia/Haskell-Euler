import Data.List

isPrime :: Int -> Bool
isPrime a = isPrimeHelper a primes

isPrimeHelper :: Int -> [Int] -> Bool
isPrimeHelper a (p:ps)
	| m - 1 /= 0 && m + 1 /= 6 = False
	| p*p > a = True
	| a `mod` p == 0 = False
	| otherwise = isPrimeHelper a ps
	where m = a `mod` 6

-- All primes under a million
primes = 2 : 3 : filter isPrime [5..1000000]

primesS = map show primes

cycles :: String -> Int -> [String]
cycles str cycle
	| length str == 1 = [str]
	| length str == cycle = []
	| otherwise     = str : (cycles (xs ++ [x]) (cycle + 1))
	where (x, xs) = (head str, tail str)

isCircular :: Int -> Bool
isCircular n = isCircularHelper (cycles (show n) 0)

isCircularHelper :: [String] -> Bool
isCircularHelper [] = True
isCircularHelper (x:xs)
	| elem x primesS = isCircularHelper xs
	| otherwise = False
          

solution :: [Int]
solution = filter isCircular primes