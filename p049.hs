
import Data.List
import Control.Monad

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

candidates = takeWhile (<10000) $ dropWhile (<1000) $ primes

isPermutation a b = (sort $ show a) ==  (sort $ show b)

solve = head $ filter (/= "148748178147") $ 
	do 
		a <- candidates
		b <- candidates 
		guard (a < b)
		let avg = (a+b) `div` 2
		guard (isPrime avg)
		guard (isPermutation a b)
		guard (isPermutation a avg)
		return (show a ++ show avg ++ show b)
