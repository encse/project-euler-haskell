import Data.Array

isDivisor a b = (a `mod` b) == 0
properDivisors n = filter (isDivisor n) [1..((n `div` 2 )+ 1)]

hasAmbientPair d n = 
		(d ! n) /= n && (d ! (d ! n)) == n

solve = sum $ filter (hasAmbientPair d) [1..10000] 
	where 
		d = listArray (1,100000) (map (sum . properDivisors) [1..100000])