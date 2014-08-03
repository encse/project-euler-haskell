import Data.Char

digitSum n = sum $ map digitToInt $ show n
solve = maximum [digitSum (a^b) | a <- [1..100], b <- [1..100]]