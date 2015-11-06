import Data.List(elemIndices, maximumBy)
import Data.Ord(compare)

triangles = [a+b+c | a <- [1..333], b <- [a..500], c <- [(b+1)..(1000-a-b)], a^2+b^2==c^2]

numOfSame :: [(Int, Int)]
numOfSame = [(length (elemIndices x triangles),x) | x <- [1..1000], length (elemIndices x triangles) /= 0]

solution = maximumBy (\x y -> compare (fst x) (fst y)) numOfSame