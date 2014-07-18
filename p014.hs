import Data.Array

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

collatzLengths citem = r
	where	
		r = listArray (1, citem) (map collatz [1..citem])
		memoized_collatz n
			| n <= citem = r ! n
			| otherwise  = collatz n
		collatz n 
			| n == 1 	= 1 
			| even n 	= 1 + memoized_collatz (n `div` 2) 
			| odd n 	= 1 + memoized_collatz (3 * n + 1)
		

maxSnd (a,b) (x,y) = if b < y then (x,y) else (a,b)

solve = fst . foldr maxSnd (0,0) $ assocs (collatzLengths 1000000)