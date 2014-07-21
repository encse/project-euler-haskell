import Data.List

nineDigitExpansion n = 
	find (\x -> length x == 9) $ takeWhile (\x -> length x < 10) $ map (foldr (++) "") $ inits $ map show $ map (* n) [1..]
	
isPandigital n = sort n == "123456789"


candidates  = 
	filter isPandigital $ map (\(Just x) -> x) $ filter foo $ map nineDigitExpansion [1..9999]
	where 
		foo (Just _) = True
	  	foo _		 = False

solve = maximum candidates