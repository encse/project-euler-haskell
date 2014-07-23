import Data.Char
import Data.List

candidates = permutations "0123456789"

a `divisible` b = a `mod` b == 0

check st = 	(mid 2) `divisible` 2 &&
			(mid 3) `divisible` 3 &&
			(mid 4) `divisible` 5 &&
			(mid 5) `divisible` 7 &&
			(mid 6) `divisible` 11 &&
			(mid 7) `divisible` 13 &&
			(mid 8) `divisible` 17 
		where 
			mid :: Int -> Int
			mid n = read (take 3 $ drop (n-1) st)

solve = sum $ map read $ filter check candidates