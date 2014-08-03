import Data.Char
import Numeric

palindrome n = let st = show n in (reverse st) == st

reverseNum :: Integer -> Integer
reverseNum n = read . reverse . show $ n;

reverseAdd n = n + reverseNum n

lychrel n = not $ any palindrome $ take 50 $ iter n
	where iter n = let r = reverseAdd n in r : iter r

solve = length $ filter lychrel [1..9999]
