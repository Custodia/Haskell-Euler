fib_mem = [fibonacci x | x <- [1..]]

fibonacci :: Int -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fib_mem !! (n - 2)) + (fib_mem !! (n - 3))

solution = (length $ takeWhile (< 10^999) fib_mem) + 1