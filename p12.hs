triangular n =  n * (n + 1) `div` 2
 
factors n = filter (\x -> n `mod` x == 0) [1..n]
factorCount = length . factors

factorCountOfTriangular n
	| even n  	= (factorCount (n `div` 2)) * (factorCount (n + 1)) 
	| otherwise = (factorCount ((n + 1) `div` 2)) * (factorCount n) 

triangularsWithEnoughDivisor d = map triangular (filter hasEnoughDivisors [1..])
 	where hasEnoughDivisors n = factorCountOfTriangular n >= d

solve = head (triangularsWithEnoughDivisor 500)