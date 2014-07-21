import Data.Char
import Numeric

toBase2 n = showIntAtBase 2 intToDigit n ""

palindrome n = let st = show n in (reverse st) == st

candidates = filter cond [1..999999]
	where cond n = (palindrome n) && (palindrome $ toBase2 n) 

solve = sum candidates