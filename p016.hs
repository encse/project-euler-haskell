import Data.Char

solve = sum . map digitToInt $ show (2 ^ 1000)