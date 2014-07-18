isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\a -> n `mod` a /= 0) [2..n-1]

primes :: [Int]
primes = 
	let 
		nemOsztja p xs = (filter (\x -> x `mod` p /= 0) xs)
		szita (p:xs) = p : (szita (nemOsztja p xs))
	in
		szita [2..]
	

primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n =

	let 
		p = head (filter (\p -> n `mod` p == 0) primes)
	in
		p : (primeDivisors (n `div` p))
		
solve =
	maximum (primeDivisors 600851475143)
	
		
