findDivisors :: Int -> [Int]
findDivisors n = [x | x <- [1..(n - 1)], n `mod` x == 0]

divByList :: Int -> [Int] -> Bool
divByList _ [] = False
divByList num (x:xs)
    | num == x = divByList num xs
	| num < (x `quot` 2) = False
	| num `mod` x == 0 = True
	| otherwise = divByList num xs

recursion :: Int -> [Int] -> [Int] -> [Int]
recursion num perfect abundant
	| num > 20161       = abundant
    | divBy2or3         = recursion (num + 1) perfect abundant
	| isMultiple        = recursion (num + 1) perfect (abundant ++ [num])
	| num  < divisorSum = recursion (num + 1) perfect (abundant ++ [num])
	| num == divisorSum = recursion (num + 1) (perfect ++ [num]) abundant
	| otherwise         = recursion (num + 1) perfect abundant
	where divBy2or3      = (num `mod` 2 /= 0) && (num `mod` 3 /= 0)
	      multOfAbundant = divByList num abundant
	      multOfPerfect  = divByList num perfect
	      isMultiple     = multOfAbundant || multOfPerfect
	      divisorSum     = sum $ findDivisors num


abundantNums = recursion 2 [] []

allSums :: [Int] -> [Int]
allSums [] = []
allSums list = (map (\x -> (head list) + x) list) ++ (allSums (tail list))

doubleAbundant = filter (< 20162) $ allSums abundantNums

solution = filter (\x -> not (elem x doubleAbundant)) [1..20161]


-- There seems to be 4994 abundant numbers