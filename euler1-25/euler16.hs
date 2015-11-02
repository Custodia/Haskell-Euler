-- Sums a given string
sumString :: String -> Int
sumString [] = 0
sumString (x:xs) = (read [x]) + (sumString xs)

-- Feels like cheating
solution = sumString (show (2^1000))