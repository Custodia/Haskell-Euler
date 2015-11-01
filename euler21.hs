findDivisors :: Int -> [Int]
findDivisors n = [x | x <- [1..(n - 1)], n `mod` x == 0]

divisorSums =  zip [sum $ findDivisors x | x <- [1..10000]] [0..9999]

amicableNums = 