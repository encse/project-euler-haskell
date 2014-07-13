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

sumPrime n = sum (takeWhile (< n) primes)
solve = sumPrime 2000000
	


	
		
