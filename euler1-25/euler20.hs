prod = product [1..100]

solution :: Int
solution = sum . map (\x -> read [x]) $ show prod

main = do
	return (solution)