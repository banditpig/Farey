module RationalTrees where
import           BinaryTrees
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Fractions

-- data BTree a = Empty | BNode a (BTree a) (BTree a)
data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]


dropFirstLast :: [a] -> [a]
dropFirstLast xs@(_:_) = tail (init xs)
dropFirstLast _        = []

sub :: Fraction -> Fraction -> Fraction
sub (F p q) (F r s) = F (p - r) (q - s)

rationalTree :: (Fraction -> Fraction) -> (Fraction -> Fraction) -> BTree Fraction
rationalTree fl fr = build (BNode (F 1 1) Empty Empty) where
    build (BNode frac Empty Empty) = build (BNode frac newLeft newRight) where
        newLeft   = BNode (fl frac) Empty Empty
        newRight  = BNode (fr frac) Empty Empty
    build (BNode frac l r)  = BNode frac (build l) (build r)

cwL :: Fraction -> Fraction
cwL (F a b) = F a (a + b)
cwR :: Fraction -> Fraction
cwR (F a b) = F (a + b) b

calkinWilfTree  = rationalTree cwL cwR

cl = take 20 $ traverseBreadthFirst calkinWilfTree

stern :: [Fraction] -> [Fraction]
stern fs = interleave fs (process fs) where
    process [_]        = []
    process (f1:f2:fs) = (f1 <> f2) : process (f2:fs)

sternBrocList :: [[Fraction]]
sternBrocList = iterate stern [F 0 1, F 1 0]


interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys


-- nearest left, value, nearest right
type Data = (Fraction, Fraction, Fraction)

mediant :: Fraction -> Fraction -> Fraction
mediant (F a b) (F c d) = F (n `div` gDiv) (d `div` gDiv) where
    n = a + c
    d = b + d
    gDiv = gcd n d



buildBrocTreeLazy :: BTree Data
buildBrocTreeLazy = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build (BNode nd@(fvw, fab, fxy) Empty Empty) = build (BNode nd newLeft newRight) where
                        newLeft   = BNode (fvw, fvw <> fab, fab) Empty Empty
                        newRight  = BNode (fab, fab <> fxy, fxy) Empty Empty
            build (BNode nd l r)  = BNode nd (build l) (build r)

buildBrocTree' :: Int -> BTree Fraction
buildBrocTree' n = treeFromList
                   . dropFirstLast           -- get rid of 0/ and 1/0
                   . last                    -- the 'depth' of resulting tree
                   . take n $ sternBrocList


buildBrocTree :: Int -> BTree Data
buildBrocTree = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build t 0  = t
            build (BNode nd@(fvw, fab, fxy) Empty Empty) n = build (BNode nd newLeft newRight) (n - 1) where
                         newLeft   = BNode (fvw, fvw <> fab, fab) Empty Empty
                         newRight  = BNode (fab, fab <> fxy, fxy) Empty Empty
            build (BNode nd l r) n = BNode nd (build l n) (build r n)

buildBrocTreeSmaller :: Int -> BTree Data
buildBrocTreeSmaller = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build t 0  = t
            build (BNode nd@(fvw, fab@(F a b), fxy) Empty Empty) n
             | a > b = Empty
             | otherwise =  build (BNode nd newLeft newRight) (n - 1) where
                         newLeft   = BNode (fvw, fvw <> fab, fab) Empty Empty
                         newRight  = BNode (fab, fab <> fxy, fxy) Empty Empty
            build (BNode nd l r) n = BNode nd (build l n) (build r n)


sternPath :: Fraction -> ([Fraction], SternPath)
sternPath frac = (reverse fPath, reverse sPath) where
    (fPath, sPath) = go buildBrocTreeLazy fr  ([],[]) where
        fr = reduce frac
        go (BNode (_, F p q, _) l r) (F n d) (frs,path)
            | p == n && q == d   = (F p q : frs, path)
            | F p q < F n d      = go r fr (F p q : frs, R : path)
            | otherwise          = go l fr (F p q : frs, L : path)

sternPathFloat :: Float -> SternPath
sternPathFloat  = go  where
    go d
        | d < 1 = L : go (d / (1 - d))
        | otherwise = R : go (d - 1)

fractionPathString :: String -> [Fraction]
fractionPathString  = fractionPath . fmap (\x -> if x == 'L' then L else R)


fractionPath :: SternPath -> [Fraction]
fractionPath  = go buildBrocTreeLazy  where
    go :: BTree Data -> SternPath -> [Fraction]
    go (BNode (_, frac, _) _ _) []     = [frac]
    go (BNode (_, frac, _) l r) (p:ps) = frac : go (pick p l r) ps where
        pick p l r = if p == L then l else r

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld = ldf 2

ldf :: Integer -> Integer -> Integer
ldf k n
 | divides k n = k
 | k*k  > n = n
 | otherwise = ldf (k+1) n

factors :: Integer -> [Integer]
factors 1 = []
factors n  = p : factors (div n p) where p = ld n

uniqueFactors :: Integer -> [Integer]
uniqueFactors = nub . factors

phi :: Integer -> Integer
phi n = n * a `div` b  where
    (a, b) = foldr f  (1, 1)  (uniqueFactors n)
    f x (num, den)  = (num * (x -1), den * x)
