digits = ['1'..'9']

similar :: String -> String -> Bool
similar x y
	| x == y = False
	| x1 == y1 = True
	| x1 == y2 = True
	| x2 == y1 = True
	| x2 == y2 = True
	| otherwise = False
	where x1 = x !! 0
	      x2 = x !! 1
	      y1 = y !! 0
	      y2 = y !! 1

readF :: String -> Float
readF x = read x

isCurious :: String -> String -> Bool
isCurious x y
	| x1 == y1 = (x2n / y2n) == (xn / yn)
	| x1 == y2 = (x2n / y1n) == (xn / yn)
	| x2 == y1 = (x1n / y2n) == (xn / yn)
	| x2 == y2 = (x1n / y1n) == (xn / yn)
	| otherwise = error "These values shouldn't be called"
	where x1 = x !! 0
	      x2 = x !! 1
	      y1 = y !! 0
	      y2 = y !! 1
	      (x1n, x2n, y1n, y2n) = (readF [x1], readF [x2], readF [y1], readF [y2])
	      (xn, yn) = (readF x, readF y) 

doubledigits = [x : [y] | x <- digits, y <- digits]

similarDoubles = filter (\x -> similar (fst x) (snd x)) [(x, y) | x <- doubledigits, y <- doubledigits, show x < show y]

curiousFractions = filter (\x -> isCurious (fst x) (snd x)) similarDoubles