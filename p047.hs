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
primesLt n = takeWhile (<n) primes

isDivisor a b = (a `mod` b) == 0

primeFactors n =  nub $ primeFactorsI n
	where primeFactorsI n
		| isPrime n = [n]
		| otherwise = 
				let ph = head $ filter (isDivisor n) $ takeWhile (< n `div` 2 + 1) primes 
				in 
					ph : (primeFactorsI (n `div` ph))


check n = 
	let 
		pf1 = primeFactors n
		pf2 = primeFactors (n + 1)
		pf3 = primeFactors (n + 2)
		pf4 = primeFactors (n + 3)
	in
		length pf1 == 4 && length pf2 == 4 && length pf3 == 4 && length pf4 == 4 && length (nub pf1++pf2++pf3++pf4) == 16 

solve = head $ filter check [2..]