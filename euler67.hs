import System.IO

-- Reduces two given rows into a row that is
-- as long as slast and contains the max sums
-- of possible paths so far (from bottom to up)
reduceRows :: [Int] -> [Int] -> [Int]
reduceRows last slast = [max (last !! x + slast !! x) (last !! (x + 1) + slast !! x) | x <- [0..(length slast - 1)]]
 
-- Recursively solves the problem by reducing rows bottom to top.
recursion :: [Int] -> [[Int]] -> Int
recursion last (x:xs)
	| length next == 1 = head next
	| otherwise        = recursion next xs
	where next = reduceRows last x

-- Helper function to turn parsed Strings to Ints
readStruct :: [[String]] -> [[Int]]
readStruct x = map (map read) x

-- Main loop
main = do
    file <- readFile "triangles67.txt"
    let triangle = readStruct (map words (lines file))
    return (recursion (head (reverse triangle)) (drop 1 (reverse triangle)))