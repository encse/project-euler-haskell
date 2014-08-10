
import Data.Array
import qualified Data.PQueue.Prio.Min as Psq
import Data.Maybe
import qualified Data.Map as Map
import Data.Function.Memoize
import Debug.Trace
import qualified Data.Numbers.Primes as P


isPrime = memoize P.isPrime
primes = P.primes

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



check' a b = (isPrime $ concat a b) && (isPrime $ concat b a) 
  	where 
  		concat a b = a * (shift b) + b
  		shift n = 10 ^ (length . show $ n)
{-			if      n < 10 then 10
			else if n < 100 then 100
			else if n < 1000 then 1000
			else if n < 10000 then 10000
			else if n < 100000 then 100000
			else if n < 1000000 then 1000000
			else if n < 10000000 then 10000000
			else if n < 100000000 then 100000000
			else if n < 1000000000 then 1000000000
			else if n < 10000000000 then 10000000000
			else if n < 100000000000 then 100000000000
			else if n < 1000000000000 then 1000000000000
			else if n < 10000000000000 then 10000000000000
			else if n < 100000000000000 then 100000000000000
			else error "too big"
-}
foo (x:xs) = (fromList (x:xs)) : (foo xs)

generateCandidates input trace =
		let 
			cwe = head input 
			qNext = Psq.fromList [((key cwe), cwe)]
		in 
			generateCandidates' qNext (tail input) trace
	
generateCandidates' q input trace =

		let 
			cweInput = head input
			(cweMin, q') = fromJust $ Psq.minView q
			z = zuzu cweMin
			sz = s z

		in 
			--trace (sz, key cweMin, Psq.size q, candidates cweMin,  s cweInput, candidates cweInput, sz > s cweInput) $
			if (sz > s cweInput) then
				let 
					qAdjusted = (Psq.insert (key cweInput) cweInput q)
					inputAdjusted = tail input
				in 
					generateCandidates' qAdjusted inputAdjusted trace
			else 
				let 
					cweMin' = step cweMin
					qNext = Psq.insert (key cweMin') cweMin' q'
				in
					if check z then 
						z : (generateCandidates' qNext input trace)
					else
						generateCandidates' qNext input trace
			

notrace _ x = x 
level0 = foo $ tail primes
level1 = generateCandidates level0 notrace
level2 = generateCandidates level1 notrace
level3 = generateCandidates level2 notrace
level4 = generateCandidates level3 notrace


main :: IO ()
main = putStrLn $show$head $  map (sum.candidates) level4


--zuzi = filter checkAll $ map toPrimes [1..]
