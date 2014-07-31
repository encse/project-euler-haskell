import Data.List

choose n 0 = 1
choose n k = (choose (n-1) (k-1)) * n `div` k

items = [choose n r |  n <- [1..100], r <-[0..n]]

solve = length $  filter (> 1000000) $ items