import Data.List



isPrime 1 = False
isPrime 2 = True
isPrime n
	| n < 1 = False
isPrime n 
	| n `mod` 2 == 0 = False 
isPrime n = 
	let 
		root = floor . sqrt . fromIntegral $ n  
		lt = takeWhile (<= root) [3,5..]
		notDivisor a = n `mod` a /= 0   
	in
		all notDivisor lt

isDivisor a b = (a `mod` b) == 0

gen a b n = n ^ 2 + a * n + b
generators = [(a*b, gen a b) | b <- [2..999], isPrime b,  a <- [-b ..999]]

value gen = length $ takeWhile (isPrime . gen) [0..]

withValues = map (\(ab, gen) -> (ab, value gen)) generators 

max2 (a,b) (x,y) = if b>y then (a,b) else (x,y)

solve = fst $ foldr1 max2 withValues