import qualified Data.HashSet as HashSet

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

isAbundant n = (d n) > n
	
abundants = filter isAbundant [1..]

isSumOfAbundants n hlmAbundants abundants = 
	any (\x -> isAbundant (n - x)) (takeWhile (<= n `div` 2) abundants)
	where
		isAbundant x = HashSet.member x hlmAbundants 

solve =  sum [x | x <- [1..28123], not (isSumOfAbundants x hlmSmallAbundants smallAbundands)]
	where 
		smallAbundands = takeWhile (< 28123) abundants
		hlmSmallAbundants = HashSet.fromList smallAbundands




