

import Data.List
import Data.Char

magic3gon = 
	[
		[x1,x2,x3, x4,x3,x5, x6,x5,x2] | 
			[x1,x2,x3,x4,x5,x6] <- permutations [1..6],
			let d = x1 + x2 + x3,
			d == x4 + x3 + x5,
			d == x6 + x5 + x2,
			x1 < x4,
			x1 < x6
	]


magic5gon = 
	[
		[x1, x6, x7,  x2, x7, x8,  x3, x8, x9,  x4, x9, x10,  x5, x10, x6] | 
			[x1,x2,x3,x4,x5,x6,x7,x8,x9,x10] <- permutations [1..10],
			x1 == minimum [x1, x2, x3, x4, x5],
			let d = x1 + x6 + x7,
			d == x2 + x7 + x8,
			d == x3 + x8 + x9,
			d == x4 + x9 + x10,
			d == x5 + x10 + x6
	]

--stringSolutions :: [String]
stringSolutions = map (concat . map show )  magic5gon

solve = maximum $ filter (\x -> 16 == length x) stringSolutions

--permutations [1..6]