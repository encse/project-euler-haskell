import qualified Data.Set as Set
import qualified Data.Numbers.Primes as P
import Data.Function.Memoize

isPrime = P.isPrime
primes = takeWhile (<30000) P.primes

check a b = (isPrime $ concat a b) && (isPrime $ concat b a) 
	where 
		concat a b = a* (10 ^ (length.show $ b)) + b 
	 
extensions = memoize extensions'

extensions' :: Int -> Set.Set Int
extensions' p = Set.fromList $ filter (check p) primes 

solve = [ a+b+c+d+e | 
			a <- primes,

			let z0 = extensions a,
			b <- Set.toList z0,

			let z1 = Set.intersection z0 (extensions b),
			c <- Set.toList z1,

			let z2 = Set.intersection z1 (extensions c),
			d <- Set.toList z2,

			let z3 = Set.intersection z2 (extensions d),
			e <- Set.toList z3
		]

main :: IO()
main = print $ minimum $ solve