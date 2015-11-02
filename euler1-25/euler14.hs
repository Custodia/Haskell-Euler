elemIndex :: Int -> [Int] -> Int
elemIndex n [] = error "Not found/cannot be called on empty list."
elemIndex n (x:xs)
	| x == n = 0
	| otherwise = (elemIndex n xs) + 1

memoized_collatz = (map collatz [1,3..999999])

solution = (elemIndex (maximum memoized_collatz) memoized_collatz) + 1

collatz :: Int -> Int
collatz n
	| n == 1    = 1
	| bytwo     = collatz (n `quot` 2) + 1
	| otherwise = collatz (3 * n + 1) + 1
    where bytwo = ((n `mod` 2) == 0)