
import Data.List
triangle n = n*(n+1) `div` 2



solveEq (a,b,c) = 
	let 
		discriminant = (b*b-4*a*c)
	in
		if discriminant < 0 then []
		else if discriminant == 0 then [-b / (2 * a)]
		else [(-b - sqrt(discriminant))/(2*a), (-b + sqrt(discriminant))/(2*a)]

isInt x = x == fromInteger (round x)

isPentagonal k = any isInt $ filter (>0) (solveEq (3.0, -1.0, -2.0 * fromInteger k))

-- 2n^2 - n -k == 0
isHexagonal k = any isInt $ filter (>0) (solveEq (2.0, -1.0, -(fromInteger k)))


solve = (filter isPentagonal $  filter isHexagonal $ map triangle [1..]) !! 2