import Data.Char

solve = 
	let 
		l = take 1000000 $ foldr1 (++) $ map show [1..]
		d n = digitToInt (l !! (n-1))
	in (d 1) * (d 10) * (d 100) * (d 1000) * (d 10000) * (d 100000) * (d 1000000) 