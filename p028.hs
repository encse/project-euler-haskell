import Data.List


differences = 1 : (foldr1 (++) $ map (replicate 4) [2,4..])

diagonals n (x:xs) = (n+x) : diagonals (n+x) xs


sumdiag n = sum $ take (2 * n -1) $ diagonals 0 differences 

solve = sumdiag 1001