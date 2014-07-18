sumOfSquares n = sum [a*a | a <- [1..n]]
squareOfSum n = let s = sum [1..n] in s*s
diff n = (squareOfSum n) - (sumOfSquares n)
solve = diff 100
	