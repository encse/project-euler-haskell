
import Data.List

isPrime :: Integer -> Bool
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

notEmpty xs = length xs > 0

indicesOfSameDigits n = 
	let 
		st = show n
		duplicates = map (\d -> elemIndices d st) ['0'..'9']
	in filter notEmpty duplicates

relevantIndexSubsets n = 
	let subsetsByDigit = map subsequences $ indicesOfSameDigits n
	in filter notEmpty $ foldr (++) [] $ subsetsByDigit

replaceWithDigits :: Integer -> [Int] -> [Integer]
replaceWithDigits n rgich = [replaceWithDigit d | d <- ['0'..'9'], d /= '0' || not (elem 0 rgich)]
	where replaceWithDigit d = read 
		[ if (elem ich rgich) then d else (st !! ich) |	let st = show n, ich <- [0.. (length st)-1]]

maximum' [] = 0
maximum' xs = maximum xs
allNeighbours p = filter (\r -> (r !! 0) == p) $ map (replaceWithDigits p) (relevantIndexSubsets p)
countPrimes xs = length . filter isPrime $ xs
primeCountInNeighbour p = map countPrimes (allNeighbours p)


solve = fst $ head $ filter (\(a,b) -> b == 8) $ zip primes (map maximum' $ map primeCountInNeighbour primes )
