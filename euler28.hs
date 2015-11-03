recursion :: Int -> [Int] -> [Int]
recursion ring list
	| ring == 500 = []
	| otherwise   = first : second : third : fourth : (recursion (ring + 1) nextList)
	where spaces = 1 + (2 * ring)
	      first  = list !! spaces
	      second = list !! (2*spaces + 1) 
	      third  = list !! (3*spaces + 2)
	      fourth = list !! (4*spaces + 3)
	      nextList = drop (4*spaces + 4) list


solution = sum $ 1 : (recursion 0 [2..])