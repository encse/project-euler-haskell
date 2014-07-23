import Data.List

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


pandigitals n = map toInt $ permutations [1..n]
	where toInt xs = foldl (\acc x-> 10 * acc + x) 0 xs

solve = maximum $ filter isPrime $ foldl1 (++) $ map pandigitals [1..9]