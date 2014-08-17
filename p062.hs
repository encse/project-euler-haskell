import qualified Data.MultiSet as M
import Data.List

toKey x = sort $ show $ x^3

lim = 100000

cubePerms = foldl p M.empty [1..lim]
	where p set x = M.insert (toKey x) set

cubeWithOccurences = map lookup [1..lim]
	where lookup x = (x^3, M.occur (toKey x) cubePerms) 

solve = fst $head $ filter (\(x,c) -> c==5) cubeWithOccurences
