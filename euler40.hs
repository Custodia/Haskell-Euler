iString = concat [show x | x <- [0..]]

d :: Int -> Int
d n = read [iString !! n]

solution = product [d (10^x) | x <- [0..6]]