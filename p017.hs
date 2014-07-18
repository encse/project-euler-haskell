import Data.Char

display 1 = "one"
display 2 = "two"
display 3 = "three"
display 4 = "four"
display 5 = "five"
display 6 = "six"
display 7 = "seven"
display 8 = "eight"
display 9 = "nine"
display 10 = "ten"
display 11 = "eleven"
display 12 = "twelve"
display 13 = "thirteen"
display 14 = "fourteen"
display 15 = "fifteen"
display 16 = "sixteen"
display 17 = "seventeen"
display 18 = "eighteen"
display 19 = "nineteen"
display 20 = "twenty"
display 30 = "thirty"
display 40 = "forty"
display 50 = "fifty"
display 60 = "sixty"
display 70 = "seventy"
display 80 = "eighty"
display 90 = "ninety"
display 1000 = "one thousand"
display n
	| n >= 100 = 
		display (n `div` 100) ++ 
		" hundred" ++  
		if (n `mod` 100) > 0 then " and " ++ display (n `mod` 100) else ""
	| n >= 20 = 
		display (n `div` 10 * 10) ++ 
		if (n `mod` 10) > 0 then "-" ++ display (n `mod` 10) else ""


solve = sum $ map (length . filter isAlpha . display) [1..1000]

