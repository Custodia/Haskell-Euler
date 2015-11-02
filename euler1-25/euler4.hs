-- check if a given number is a palindrome.
checkPal :: Int -> Bool
checkPal n = show n == reverse (show n)

-- Get all the palindromes
paliProducts = [x * y | x <- [100..999], y <- [100..x], checkPal (x * y)]

-- Choose the biggest one.
biggestPaliProduct = maximum paliProducts