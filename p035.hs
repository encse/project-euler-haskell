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

candidates = takeWhile (< 1000000) primes


circulars :: Int -> [Int]
circulars n = 
	let st = show n 
	in
		(map read $ map (\(x,y) -> x++y) $ zip (tails st) (inits st)) 

isCircular n = all isPrime $ circulars n
solve = length $ filter isCircular candidates