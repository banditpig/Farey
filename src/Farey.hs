
import           Graphics.Gloss

data Fraction = F Int Int
instance Show Fraction where
    show (F n d) = show n ++ "/" ++ show d
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

-- nearest left, value,, nearest right
type Data = (Fraction, Fraction, Fraction)

buildBrocTree :: Int -> BinTree Data
buildBrocTree = build (Node (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build :: BinTree Data -> Int -> BinTree Data
            build t 0  = t
            build (Node nd@(F v w, F a b, F x y) Empty Empty) n = build (Node nd newLeft newRight) (n - 1) where
                    newLeft  = Node ( F v w , F (a + v) (b + w), F a b ) Empty Empty
                    newRight = Node ( F a b , F (a + x) (b + y), F x y ) Empty Empty
            build (Node nd l r) n = Node nd (build l n) (build r n)

traverseBFirst :: BinTree Data -> [Fraction]
traverseBFirst tree = go [tree]
    where
        go [] = []
        go xs = fmap nodeVal xs ++ go (concatMap lrSubTrees xs)
        nodeVal (Node (x, v, z) _ _) = v
        lrSubTrees (Node _ Empty Empty) = []
        lrSubTrees (Node _ Empty b)     = [b]
        lrSubTrees (Node _ a Empty)     = [a]
        lrSubTrees (Node _ a b)         = [a,b]

add :: Fraction -> Fraction -> Fraction
add (F n d) (F n' d') =  reduce $ F (n + n') (d + d')

reduce :: Fraction  -> Fraction
reduce (F n d) = F (n `div` gc ) (d `div` gc) where gc = gcd n d :: Int

farey :: Int -> [(Int, Int)]
farey n = farey' (0, 1) (1, n) where
    farey' (a, b) (c, d)
        | p == 1 && q == 1 = [(1,1)]
        | otherwise = (a, b) : farey' (c, d) (p, q ) where
            n' = floor $ fromIntegral (n + b) / fromIntegral d
            p = n' * c - a
            q = n' * d - b

 -- radius 1/(2q2) and centre at (p/q, 1/(2q2))
fordCircle :: (Int, Int) -> (Double, Double,  Double)
fordCircle (p, q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q*q)

fordCircles = fmap fordCircle . farey


window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing
