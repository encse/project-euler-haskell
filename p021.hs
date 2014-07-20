import Data.Array

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | n `mod` 2 == 0 = False 
isPrime n = 
	let 
		root = floor . sqrt . fromIntegral $ n  
		lt = takeWhile (<= root) [3,5..]
		notDivisor a = n `mod` a /= 0   
	in
		all notDivisor lt

isDivisor a b = (a `mod` b) == 0

divisors n = filter (isDivisor n) [2..((n `div` 2 )+ 1)]

smallestDivisor n = head $ divisors n

multiplicityOfDivisor n p = maximum $ takeWhile (\x -> isDivisor n (p ^ x)) [1..] 

d 1 = 0
d n 
	| isPrime n	= 1
	| otherwise =
		let 
			p = smallestDivisor n
			m = multiplicityOfDivisor n p
			s = sum $ map (p ^ ) [0..m]
			rem = n `div` (p ^ m)
		in
			((d rem) + rem) * s - n

hasAmbientPair n = 
		(d n) /= n && (d (d n)) == n

solve = sum $ filter (hasAmbientPair) [2..10000] 
