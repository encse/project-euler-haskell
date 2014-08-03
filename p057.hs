import Data.Ratio


a = as (1 % 2)
	where as n = (1 + n) : as (1 / (2 + n))

solve = length $ filter bigger $ take 1000 a
	where bigger n = (length $ show $ numerator n) > (length $ show $ denominator n)