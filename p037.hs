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

isTruncatableI trunc n = all isPrime $ map read $ filter (/= "") $ trunc (show n)

isTruncatableLeft n = isTruncatableI tails n
isTruncatableRight n = isTruncatableI inits n

isTruncatable n = (isTruncatableLeft n) && (isTruncatableRight n)

candidates = take 11 $ filter isTruncatable $ filter (>7) primes
solve = sum candidates
	
		
