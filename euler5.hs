-- Abstraction for the solution given N.
solution n = recursion n 1

-- The recursive formula for solution.
recursion :: Int -> Int -> Int
recursion cur res
	| cur == 1 = res
	| otherwise = recursion (cur - 1) (res * fix)
	where gcds = gcd cur res
	      fix  = cur `quot` gcds