
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
 
primeAndSeqLength [] limit = []
primeAndSeqLength primes limit = 
	filter (\(a,b) -> isPrime a) $ 
	takeWhile (\(a,b) -> a < limit) $ 
	primeAndSeqLengthI primes 0 0
	where 
		primeAndSeqLengthI [] _ _ = []
		primeAndSeqLengthI (p:primes) sum length = (sum + p, length + 1) : (primeAndSeqLengthI primes (sum + p) (length + 1))

allPrimeAndSeqLength [] _ = []
allPrimeAndSeqLength primes limit = (primeAndSeqLength primes limit) ++ (allPrimeAndSeqLength (tail primes) limit)

max2 (a,b) (x,y) = if (b<y) then (x,y) else (a,b)

solveN n = foldr1 max2 $ allPrimeAndSeqLength (takeWhile (<n) primes) n

solve = solveN 1000000
	