
import Data.Ratio
import Data.Char

esteps :: [Ratio Integer]
esteps = 2 : (concat $ [[1,2*x,1] | x<-[1..]])

sumSteps :: [Ratio Integer] -> [Ratio Integer]
sumSteps (x:xs) = 
	x : (map (\s -> x + (1 / s)) $ sumSteps xs)

solve = sum $ map digitToInt $ show $ numerator  $ sumSteps esteps !! 99