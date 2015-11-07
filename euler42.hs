import Data.Char(isLetter, isSpace)
import Data.Maybe(fromJust)
import Data.List(elemIndex)

triangleHelper :: Int -> Int
triangleHelper n = round ((fromIntegral n / 2)*(fromIntegral n + 1))

triangleNums = [triangleHelper x | x <- [1..]]

isTriangleNum :: Int -> Bool
isTriangleNum n
	| elem n nums = True 
	| otherwise   = False 
	where nums = takeWhile (<=n) triangleNums

charScore :: Char -> Int
charScore char
	| charS == Nothing = error "char not in scope"
	| otherwise        = (fromJust charS) + 1
	where charS = elemIndex char ['A'..'Z']

wordScore :: String -> Int
wordScore []     = 0
wordScore (x:xs) = (charScore x) + (wordScore xs)

main = do
	file <- readFile "words42.txt"
	let wordss = words . filter (\x -> (isLetter x) || (isSpace x)) $ map (\x -> if (x == ',') then ' ' else x) file
	let triangleWords = filter isTriangleNum $ map wordScore wordss
	return (length triangleWords)