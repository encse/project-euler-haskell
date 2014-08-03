
import Data.Ratio
import Data.List
import Data.Maybe

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

diagonals = diagonals' 0 differences 
	where 
		diagonals' n (x:xs) = (n+x) : diagonals' (n+x) xs
		differences = 1 : (foldr1 (++) $ map (replicate 4) [2,4..])

primeRatio = tail $ ratio' 0 0 diagonals
	where ratio' cPrime cItem (x:xs) = (cPrime % cItem) : (ratio' cPrimeNext (cItem+1) xs)
		where cPrimeNext = if (isPrime x) then (cPrime + 1) else cPrime

fullCircles (x : xs) = x : fullCircles (drop 3 xs) 

ratioInCirclesBelow = map ( < 0.1) (fullCircles primeRatio)

circleIndex = fromJust $ elemIndex True $ tail ratioInCirclesBelow

solve = 1 + (1 + circleIndex) * 2 
--solve = sumdiag 1001