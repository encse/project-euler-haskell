import Data.List


triangles n = [(a,b,c)| a <- [1..(n `div` 2)], b<-[a..(n `div` 2)], let c = n-a-b, c*c == a*a + b*b ]

candidates = map triangles [1..1000]

max2 (a,b) (x,y) = if b > y then (a,b) else (x,y)

solve = fst $ foldr1 max2 $ zip [1..] (map length candidates)