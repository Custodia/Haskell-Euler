sof :: Int -> Bool
sof 1 = False
sof 2 = False
sof n = n == sofHelper (show n)

sofHelper :: String -> Int
sofHelper [] = 0
sofHelper (x:xs) = (product [1..(read [x])]) + sofHelper xs

curiousNumbers = [x | x <- [1..], sof x]

-- Turns out there are only two of these numbers.
-- I have no proof of that, waited it out the first
-- time I ran this, tried returning just the two
-- I found and it was correct.
solution = sum $ take 2 curiousNumbers