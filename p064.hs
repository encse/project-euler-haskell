
import Data.List
import Data.Maybe

data Expr = Expr Int Int Int Int Int deriving (Eq)

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


instance Show Expr where
    show (Expr a b c d n) = "(" ++ showI a ++ showX b ++ ") / (" ++ showI c ++ showX d ++ ")"
   		where 
   			showI x = show' x ++ "√"++ show n++""
   			showX x 
   				| x < 0 = " - " ++ show' (-x)
   				| otherwise = " + " ++  show' x
   			
   			show' x = show x


step e@(Expr a b c d n) = 
	let 
		r@(Expr a' b' 0 d' _) = rationalize e
		intPart = floor $ toDouble r
		remainder = simplify (Expr a' (b' - intPart * d') 0 d' n)
	in
		(intPart, remainder)

--isSquare :: Int -> Bool
isSquare n = (floor $ sqrt $ fromIntegral $ n) ^  2 == n

steps e esSeen 
	| elem e esSeen = []
	| otherwise = 
		let (n, e') = step e
			in (n, e') : steps (reciprocal e') (e : esSeen)

findLoopStart steps =
	let 
		exprs  = map snd steps
		exprLoop = last exprs 
	in 
		1 + (fromJust $ elemIndex exprLoop exprs)

loopLength steps = (length steps) - (findLoopStart steps)

toContinuedFractionSteps n = 
	steps (Expr 1 0 0 1 n) []

solve = length $  filter odd $ map (loopLength . toContinuedFractionSteps) $ filter (not . isSquare) [1..10000]