-- Turns a char into a int
readInt :: Char -> Int
readInt c = read [c]

-- Given a number under 1000 gives out its string.
intToString :: Int -> String
intToString num
    | num > 1000 || num < 0 = error "Invalid number"
    | num == 0 = ""
    | num == 1 = "one"
    | num == 2 = "two"
    | num == 3 = "three"
    | num == 4 = "four"
    | num == 5 = "five"
    | num == 6 = "six"
    | num == 7 = "seven"
    | num == 8 = "eight"
    | num == 9 = "nine"
    | num == 10 = "ten"
    | num == 11 = "eleven"
    | num == 12 = "twelve"
    | num == 13 = "thirteen"
    | num == 15 = "fifteen"
    | num == 18 = "eighteen"
    | num <  20  = (intToString (readInt (str !! 1))) ++ "teen"
    | num <  30  = "twenty-" ++ (intToString (readInt (str !! 1)))
    | num <  40  = "thirty-" ++ (intToString (readInt (str !! 1)))
    | num <  50  = "forty-" ++ (intToString (readInt (str !! 1)))
    | num <  60  = "fifty-" ++ (intToString (readInt (str !! 1)))
    | num <  70  = "sixty-" ++ (intToString (readInt (str !! 1)))
    | num <  80  = "seventy-" ++ (intToString (readInt (str !! 1)))
    | num <  90  = "eighty-" ++ (intToString (readInt (str !! 1)))
    | num <  100 = "ninety-" ++ (intToString (readInt (str !! 1))) 
    | num == 1000 = "one thousand"
    | twol == "00" = (intToString (readInt (str !! 0))) ++ " hundred"
    | otherwise    = (intToString (readInt (str !! 0))) ++ " hundred and " ++ (intToString ((read twol) :: Int))
    where str  = show num
          len  = length str
          twol = (str !! 1) : [str !! 2]

-- Recursively adds intToString 1..1000 together
recursion :: Int -> String
recursion n
	| n == 1000 = intToString n
	| otherwise = intToString n ++ (recursion (n + 1))

-- Filters out spaces and hyphens from a string.
filters :: String -> String
filters [] = []
filters (x:xs)
	| x == ' '  = filters xs
	| x == '-'  = filters xs
	| otherwise = x : (filters xs)

solution = length (filters (recursion 1))