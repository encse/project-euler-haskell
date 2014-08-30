
import Data.List
import Data.Maybe
import Data.Ratio

data Expr = Expr Integer Integer Integer Integer Integer deriving (Eq)

instance Show Expr where
    show (Expr a b c d n) = "(" ++ showI a ++ showX b ++ ") / (" ++ showI c ++ showX d ++ ")"
   		where 
   			showI x = show' x ++ "√"++ show n++""
   			showX x 
   				| x < 0 = " - " ++ show' (-x)
   				| otherwise = " + " ++  show' x
   			show' x = show x

mul (Expr a b c d n1) (Expr a' b' c' d' n2) 
	| n1 == n2 =  Expr (a*b' + a'*b) (a*a'*n1 + b*b') (c*d' + c'*d) (c*c'*n1 + d*d') n1

reciprocal (Expr a b c d n) = Expr c d a b n

rationalize e@(Expr _ _ 0 _ _) = e
rationalize e@(Expr a b c d n) = 
	simplify $ mul e (Expr c (-d) c (-d) n)

toDouble (Expr a b c d n) = 
	let 
		a' = fromIntegral a
		b' = fromIntegral b
		c' = fromIntegral c
		d' = fromIntegral d
		n' = fromIntegral n
	in
		(a' * (sqrt n') + b') / (c' * (sqrt n') + d')

gcd' xs = foldr1 gcd xs
simplify (Expr a b c d n) = 
	let g = gcd' [a,b,c,d]
	in Expr (a `div` g) (b `div` g) (c `div` g) (d `div` g) n

step e@(Expr a b c d n) = 
	let 
		r@(Expr a' b' 0 d' _) = rationalize e
		intPart = floor $ toDouble r
		remainder = simplify (Expr a' (b' - intPart * d') 0 d' n)
	in
		(intPart, remainder)

isSquare n = (floor $ sqrt $ fromIntegral $ n) ^  2 == n

steps e  =
	let (n, e') = step e
	  in (n, e') : steps (reciprocal e') 

toContinuedFractionSteps n = 
	map (\x -> fst x % 1) $ steps (Expr 1 0 0 1 n)


sumSteps :: [Ratio Integer] -> [Ratio Integer]
sumSteps (x:xs) = 
	x : (map (\s -> x + (1 / s)) $ sumSteps xs)

-- returns the fundamental solution (smallest in x) to the x^2 - D*y^2 = 1 Pell equation
-- http://en.wikipedia.org/wiki/Pell's_equation
solvePell d = head $ filter isSolution approx
	where
		approx = sumSteps $ toContinuedFractionSteps d
		isSolution a = (numerator a)^2 - d * (denominator a)^2 == 1

minX d = numerator $ solvePell d

solve = fst $ foldr1 (\(maxD, maxX) (d,x) -> if (x > maxX) then (d,x) else (maxD, maxX)) [(d, minX d) | d <- [1..1000], not $ isSquare d]



