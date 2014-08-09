
import Data.Array
import qualified Data.PQueue.Prio.Min as Psq
import Data.Maybe
import qualified Data.Map as Map
import Data.Function.Memoize
import Debug.Trace

isPrime :: Int -> Bool
isPrime = memoize isPrime'
isPrime' k = null [ x | x <- 2 : [3,5..isqrt k], k `mod`x  == 0]

isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)

primes = [2,3,5] ++ filter isPrime [7,9..]

data Cwe = Cwe { 
	s :: Int,
	candidates :: [Int],  
	extensions :: [Int]}

fromList xs = Cwe {
	candidates = [head xs], 
	extensions = tail xs,
	s  = head xs
}

key cwe = (s cwe) + (head $ extensions cwe)

step cwe = 
	let 
		candidates' = candidates cwe
		extensions' =  tail $ extensions cwe
		s' = s cwe  
	in
		Cwe {
			candidates = candidates', 
			extensions = extensions',
			s = s'
		}


zuzu cwe = 
	let 
		nextNum = head $ extensions cwe
		candidates' = nextNum : candidates cwe
		extensions' = tail $ extensions cwe
		s' = s cwe + nextNum 
	in
		Cwe {
			candidates = candidates',
			extensions = extensions',
			s = s'
		}

check cwe = 
	let (p:ps) = candidates cwe 
	in all (\pT -> check' p pT) ps  

cch n = 
	if n < 10 then 1
	else if n<100 then 2
	else if n <1000 then 3
	else if n <10000 then 4
	else 1 + cch (n `div` 10)

check' a b = (isPrime $ concat a b) && (isPrime $ concat b a) 
	  	where 
	  		concat a b =
	  		  --  a* (10 ^ (length.show $ b)) + b 
	  			concat a b b
	  			where 
	  				concat a b 0 = a+b
	  				concat a b x = concat (a*10) b (x `div` 10) 

foo (x:xs) = (fromList (x:xs)) : (foo xs)

generateCandidates q input trace
	| Psq.null q = 
		let 
			cwe = head input 
			qNext = Psq.insert (key cwe) cwe q
		in 
			generateCandidates qNext (tail input) trace
	
	| otherwise = 
		let 
			cweInput = head input
			(cweMin, q') = fromJust $ Psq.minView q
			z = zuzu cweMin
			sz = s z

		in 
			trace (sz, Psq.size q, candidates cweMin,  s cweInput, candidates cweInput, sz > s cweInput) $
			if (sz > s cweInput) then
				let 
					qAdjusted = (Psq.insert (key cweInput) cweInput q)
					inputAdjusted = tail input
				{-	(qAdjusted, inputAdjusted) = adjust q input
						where
							adjust q input =
								let cweInput = head input
								in
									if (sz > s cweInput) then
										adjust (Psq.insert (key cweInput) cweInput q) $ tail input
									else
										(q, input)
				-}
				in 
					generateCandidates qAdjusted inputAdjusted trace
			else 
				let 
					cweMin' = step cweMin
					qNext = Psq.insert (key cweMin') cweMin' q'
				in
					if check z then 
						z : (generateCandidates qNext input trace)
					else
						generateCandidates qNext input trace
			

notrace _ x = x 
level0 = foo $ tail primes
level1 = generateCandidates Psq.empty level0 notrace
level2 = generateCandidates Psq.empty level1 notrace
level3 = generateCandidates Psq.empty level2 notrace
level4 = generateCandidates Psq.empty level3 notrace

main :: IO ()
main = putStr $ show $ map candidates $ take 5 $ level4


checkAll ps = all (\(a,b) -> check' a b) [(p1,p2) | p1 <- ps, p2 <- ps]

toPrimes n = 
	[ [p1,p2,p3,p4,p5] | 
		p1 <- takeWhile (<n) primes, 
		p2 <- takeWhile (< (p1)) primes,
		p3 <- takeWhile (< (p2)) primes,
		p4 <- takeWhile (< (p3)) primes,
		let p5 = n - p1 - p2 - p3 -p4,
		isPrime p5
	]

--zuzi = filter checkAll $ map toPrimes [1..]
