import System.IO
import Data.Char
import Data.List
import Data.Maybe

charScore :: Char -> Int
charScore char
	| charS == Nothing = error "char not in scope"
	| otherwise        = (fromJust charS) + 1
	where charS = elemIndex char ['A'..'Z']

nameScore :: String -> Int
nameScore []     = 0
nameScore (x:xs) = (charScore x) + (nameScore xs)

main = do
	file <- readFile "euler22names.txt"
	let names     = sort . words . filter (\x -> (isLetter x) || (isSpace x)) $ map (\x -> if (x == ',') then ' ' else x) file
	let sumScores = sum . map (\x -> (nameScore (fst x) * (snd x))) $ zip names [1..]
	return (sumScores)