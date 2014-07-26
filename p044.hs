
import Data.List

solveEq (a,b,c) = 
	let 
		discriminant = (b*b-4*a*c)
	in
		if discriminant < 0 then []
		else if discriminant == 0 then [-b / (2 * a)]
		else [(-b - sqrt(discriminant))/(2*a), (-b + sqrt(discriminant))/(2*a)]

isInt x = x == fromInteger (round x)

isDivisor a b = (a `mod` b) == 0

divisors n = nub $ [1] ++ divisorsI n ++ [n]
	where 
		divisorsI n = 
			let 		
				xs = take 1 $ filter (isDivisor n) [2..n]
			in
				if length xs == 0 
				then []
				else
					let 
						x = head xs
						d = divisorsI (n `div` x)
					in
						[x] ++ d ++ (map (*x) d)

isPentagonal k = any isInt $ filter (>0) (solveEq (3.0, -1.0, -2.0 * fromInteger k))

pentagonal n = (3 * n - 1) * n `div` 2

pentagonals = map pentagonal [1..]

-- retrieve list of pentagonal pairs (a,b), for which a-b == p 
factorize :: Integer -> [(Integer,Integer)]
factorize p | isPentagonal p =  
	let 

		findNs :: Integer -> [Integer]
		findNs a = [ n | d1 <- divisors (12 * a), let d2 = (12*a) `div` d1, isDivisor (d1-d2+2) 12, let n =  (d1-d2+2) `div` 12, n>=1 ]

		findKs :: Integer -> Integer -> [Integer]
		findKs n a = [ truncate k | k <- solveEq (3.0, 6 * (fromIntegral n)-1, -(fromIntegral a)), isInt k, k > 0]
		
		rgNAndK = [(n, k) | n <- findNs (2*p), k <- findKs n (2*p)]
	in
		map (\(n,k) -> (pentagonal (n+k), pentagonal n)) rgNAndK


solve =  head [a-b | (a,b) <- foldr1 (++)  $ map factorize pentagonals, isPentagonal (a+b)]
