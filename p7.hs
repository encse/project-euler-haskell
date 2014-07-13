primes :: [Int]
primes = 
	let 
		nemOsztja p xs = (filter (\x -> x `mod` p /= 0) xs)
		szita (p:xs) = p : (szita (nemOsztja p xs))
	in
		szita [2..]
	
solve = primes !! 10000

	
		
