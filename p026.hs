import Data.List

felbont n d 
	| d > n		= felbont (10*n) d
	| (n `mod` d) == 0	= []
	| otherwise = n : (felbont (n `mod` d) d)

findloop _ [] = 0
findloop ys (x:xs)
	| elemIndex x ys /= Nothing	= let Just z = elemIndex x ys in z + 1
	| otherwise = findloop (x:ys) xs  

loopLength d = findloop [] $ felbont 1 d
max2 (a,b) (x,y) = if b>y then (a,b) else (x,y)

solve = fst $ foldr1 max2 [(d, loopLength d) | d <- [1..999] ]  