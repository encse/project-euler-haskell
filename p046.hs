import Data.List

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

primes = filter isPrime [2..]

isInt x = x == fromIntegral (round x)

isSquare n = isInt $ sqrt $ fromIntegral n

primesLt n = takeWhile (<n) primes

check n = not $ any (\p -> isSquare $ (n - p) `div` 2 ) $ primesLt n
solve = head $ filter check $ filter (not . isPrime) [2..]		
