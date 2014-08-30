import Data.Numbers.Primes 

maxProportion lim = maxProportion' primes 1 1
	where maxProportion' (p:ps) proportion n
		| n * p <= lim 	= maxProportion' ps (proportion * (fromIntegral p) / (fromIntegral (p-1))) (n * p)
		| otherwise = (n, proportion)

solve = fst $ maxProportion 1000000