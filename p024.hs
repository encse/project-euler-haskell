import Data.List
solve = (sort $ map (foldl1 (++)) $ map (map show) $ permutations [0..9]) !! 999999
