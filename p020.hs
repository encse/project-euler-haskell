import Data.Char

solve = sum $ map digitToInt $ show $ product [1..100]