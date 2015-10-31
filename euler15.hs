--memoization of the grid coordinates.
grid_mem = [ [ grid x y | x <- [0..] ] | y <- [0..]]

-- Find how many possible routes from x y to 0 0
grid :: Int -> Int -> Int
grid x y
	| x == 0 && y == 0 = 1
	| x == 0           = 1 --always only one solution
	| y == 0           = 1 --always only one solution
	| otherwise        = (grid_mem !! x !! (y - 1)) + (grid_mem !! (x - 1) !! y)

solution = grid 20 20