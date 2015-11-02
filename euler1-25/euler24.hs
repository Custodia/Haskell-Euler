import Data.List

solution :: Int
solution = (sort . map read $ permutations "0123456789") !! 999999