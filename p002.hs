
fibonacci :: [Int]
fibonacci = [1,2] ++ fibi 1 2
	where		
		fibi :: Int -> Int -> [Int] 
		fibi x1 x2 = (x1 + x2) : (fibi x2 (x1 + x2))
		
solve = 
	let 
		fs = takeWhile ( <= 4000000) fibonacci
	in
		sum [f | f <- fs, f `mod` 2 == 0]