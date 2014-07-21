
import Data.List

isPandigitalSt st = sort st == "123456789"
isPandigital (a,b,c) = isPandigitalSt (show a ++ show b ++ show c)


candidates = [ (x, y, x*y) | x <- [1..9], y <- [1000..9999]] ++
			 [ (x, y, x*y) | x <- [10..99], y <- [100..999]]

pandigitalTriples = filter isPandigital candidates
unusuals = nub $ map proj3 pandigitalTriples

proj3 (a,b,c) = c
solve = sum unusuals