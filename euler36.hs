import Numeric (showIntAtBase)
import Data.Char (intToDigit)

base2 :: Int -> String
base2 num = showIntAtBase 2 intToDigit num ""

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome (x:xs)
	| x == lastC = isPalindrome (take (length xs - 1) xs)
	| otherwise = False
	where lastC = last xs

checker :: Int -> Bool
checker num
	| orig && binary = True
	| otherwise      = False
	where orig   = isPalindrome (show num)
	      binary = isPalindrome (base2 num)

palindromes = filter checker [1..1000000]