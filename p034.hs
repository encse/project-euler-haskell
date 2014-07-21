import Data.Char

factorial n = product [1..n]
isCurious x = (sum $ map factorial $ map digitToInt $ show x) == x

--limit: the biggest candidate number with 7 digits is 7 * 9!, its not possible to generate numbers with 8 or more digits
solve = sum $ filter isCurious [3..2540160]