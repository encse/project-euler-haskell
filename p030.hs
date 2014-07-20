import Data.Char
import Data.List

sumDigit5 n = sum $ map (^ 5) $ map digitToInt $ show n

sumDigit4 n = sum $ map (^ 4) $ map digitToInt $ show n

-- limit:  9^5 * 7 is less than one million, so 
-- there is no way to produce a number greater than 1000000 with the sum of seven ^ 5 numbers,
-- the same is true for numbers with more digits
solve = sum $ filter (\n -> sumDigit5 n == n) [2 .. 999999]