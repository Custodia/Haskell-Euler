days =     [31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
daysleap = [31, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30]

firstDays :: Int -> Int -> [Int]
firstDays cur year
	| leapcentury = drop 1 . scanl (+) cur $ daysleap
	| century     = drop 1 . scanl (+) cur $ days
	| leap        = drop 1 . scanl (+) cur $ daysleap
	| otherwise   = drop 1 . scanl (+) cur $ days
	where leap        = year `mod` 4 == 0
	      century     = year `mod` 100 == 0
	      leapcentury = year `mod` 400 == 0


recursion :: Int -> [Int]
recursion year
	| year == 1901 = firstDays (-30) year
	| otherwise    = next ++ firstDays cur year
	where next = recursion (year - 1)
	      cur  = last next

main = do 
	return ((length . filter (\x -> x `mod` 7 == 0) $ recursion 2000) - 1)