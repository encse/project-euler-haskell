
import Data.Ratio

candidates = [(x,y) | x <- [10..99], y <- [x+1..99]]


isCurious (x,y) = 
	let 
		[a,b] = [x `div` 10, x `mod` 10]
		[c,d] = [y `div` 10, y `mod` 10]
	in
		not (b == 0 && d == 0) &&

		((a == c  && b * y == x * d) ||
		 (b == d  && a * y == x * c) ||
		 (a == d  && b * y == x * c) ||
		 (b == c  && a * y == x * d)) 

curiousRationals = [(toRational a) / (toRational b)  | (a,b) <- filter isCurious candidates]

solve = denominator $ product curiousRationals
