import Data.HashSet

powers :: [Integer]
powers = [a^b | a <- [2..100], b<-[2..100]]
solve = size $ fromList powers