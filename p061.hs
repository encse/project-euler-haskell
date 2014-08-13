
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe

data Kind = T | S | P | Hx | Hp | O deriving (Eq, Show)
type Seqence = [Member]
type Member = (Kind, Int)

triangles :: Seqence
triangles = seqence T (\n -> n * (n+1) `div` 2)

squares :: Seqence
squares = seqence S (\n -> n * n) 

pentagonals :: Seqence
pentagonals = seqence P  (\n -> n * (3 * n-1) `div` 2)

hexagonals :: Seqence
hexagonals = seqence Hx  (\n -> n * (2 * n-1))

heptagonals :: Seqence
heptagonals = seqence Hp  (\n -> n * (5 * n-3) `div` 2)

octagonals :: Seqence
octagonals = seqence O  (\n -> n * (3 * n-2))

seqence :: Kind -> (Int -> Int) -> Seqence
seqence kind generator  = map (\x -> (kind,x)) $ fourDigits $ map generator [1..] 
	where
		fourDigits xs = takeWhile (<10000) $ dropWhile (< 1000) xs

follower :: Member -> Member -> Bool
follower (k1, i1) (k2, i2) = k1 /= k2 && (take 2 $ show i1) == (drop 2 $ show i2)

allMembers :: [Member]
allMembers = foldr1 (++) [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]

nextMembers :: M.Map Int [Member]
nextMembers = foldl popupale M.empty allMembers
	where 
		popupale map (kind, num) = M.insert num (followers (kind, num)) map
		followers member = filter (follower member) allMembers

candidates :: [[Int]]
candidates  = 
	[ 
		[a0, a1, a2, a3, a4, a5] |
			(k0, a0) <- triangles,
			(k1, a1) <- fetchNext a0 [k0],
			(k2, a2) <- fetchNext a1 [k0, k1],
			(k3, a3) <- fetchNext a2 [k0, k1, k2],
			(k4, a4) <- fetchNext a3 [k0, k1, k2, k3],
			(k5, a5) <- fetchNext a4 [k0, k1, k2, k3, k4],
			follower (k5, a5) (k0, a0)
	]

	where
		fetchNext num kindsSeen = skipKind kindsSeen $ fromJust $ M.lookup num nextMembers
		skipKind kinds xs = filter (\(kind, _) -> not $ elem kind kinds) xs

solve :: Int
solve = sum $ head $ candidates
