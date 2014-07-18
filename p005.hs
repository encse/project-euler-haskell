lcm' :: Int -> [Int] -> Int	
lcm' n [] = n
lcm' n (x:xs) = lcm' (n * x `div` (gcd n x)) xs
			
solve = lcm' 1 [1..20]
	