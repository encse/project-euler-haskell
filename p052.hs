import Data.List

isPermutation a b = (sort $ show a) ==  (sort $ show b)

m p = map (\(a,b) -> a*b) $ zip [1..6] (repeat p)

isAllPermutation xs =  all (\(a,b) -> isPermutation a b) $ zip xs (tail xs)

solve = head $ head $ filter isAllPermutation $ map m [1..]