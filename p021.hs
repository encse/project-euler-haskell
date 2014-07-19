import Data.Array

isDivisor a b = (a `mod` b) == 0
properDivisors n = filter (isDivisor n) [1..(n-1)]

d n = sum $ properDivisors n

hasAmbientPair n = (d n) /= n && (d $ d n) == n

solve = sum $ filter hasAmbientPair [1..10000] 
