findDivisors :: Int -> [Int]
findDivisors n = [x | x <- [1..(n - 1)], n `mod` x == 0]

divisorSums = [sum $ findDivisors x | x <- [1..10000]]

isAmicable :: Int -> Bool
isAmicable num
	| divsum > 10000 = False
	| num == divsum  = False
	| amicable       = True
	| otherwise      = False
	where divsum   = divisorSums !! (num - 1)
	      amicable = num == (divisorSums !! (divsum - 1))

recursion :: Int -> [Int]
recursion num
	| num == 1  = []
	| amicable  = num : (recursion (num - 1))
	| otherwise = recursion (num - 1)
	where amicable = isAmicable num