module Farey where
import           BinaryTrees

data Fraction  = F Integer Integer
data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]

instance Show Fraction where show (F n d) = show n ++ "/" ++ show d


-- nearest left, value, nearest right
type Data = (Fraction, Fraction, Fraction)

mediant :: Fraction -> Fraction -> Fraction
mediant (F a b) (F c d) = F (n `div` gDiv) (d `div` gDiv) where
    n = a + c
    d = b + d
    gDiv = gcd n d

reduce :: Fraction -> Fraction
reduce (F p q) = F p' q'
    where p' = p `div` gDiv
          q' = q `div` gDiv
          gDiv = gcd p q

lessThan :: Fraction -> Fraction -> Bool
lessThan (F m n ) (F p q) =  m * q < n * p


buildBrocTreeLazy :: BTree Data
buildBrocTreeLazy = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build :: BTree Data -> BTree Data
            build (BNode nd@(F v w, F a b, F x y) Empty Empty) = build (BNode nd newLeft newRight) where
                        newLeft  = BNode ( F v w , F (a + v) (b + w), F a b ) Empty Empty
                        newRight = BNode ( F a b , F (a + x) (b + y), F x y ) Empty Empty
            build (BNode nd l r) = BNode nd (build l) (build r)


buildBrocTree :: Int -> BTree Data
buildBrocTree = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build :: BTree Data -> Int -> BTree Data
            build t 0  = t
            build (BNode nd@(F v w, F a b, F x y) Empty Empty) n = build (BNode nd newLeft newRight) (n - 1) where
                         newLeft   = BNode ( F v w , F (a + v) (b + w), F a b ) Empty Empty
                         newRight  = BNode ( F a b , F (a + x) (b + y), F x y ) Empty Empty
            build (BNode nd l r) n = BNode nd (build l n) (build r n)

buildBrocTreeSmaller :: Int -> BTree Data
buildBrocTreeSmaller = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build :: BTree Data -> Int -> BTree Data
            build t 0  = t
            build (BNode nd@(F v w, F a b, F x y) Empty Empty) n
             | a > b = Empty
             | otherwise =  build (BNode nd newLeft newRight) (n - 1) where
                         newLeft   = BNode ( F v w , F (a + v) (b + w), F a b ) Empty Empty
                         newRight  = BNode ( F a b , F (a + x) (b + y), F x y ) Empty Empty
            build (BNode nd l r) n = BNode nd (build l n) (build r n)


sternPath :: Fraction -> ([Fraction], SternPath)
sternPath frac = (reverse fPath, reverse sPath) where
    (fPath, sPath) = go buildBrocTreeLazy fr  ([],[]) where
        fr = reduce frac
        go (BNode (_, F p q, _) l r) (F n d) (frs,path)
            | p == n && q == d = (F p q : frs, path)
            | lessThan (F p q) (F n d) = go r fr (F p q : frs, R : path)
            | otherwise                = go l fr (F p q : frs, L : path)


fractionPath :: SternPath -> [Fraction]
fractionPath  = go buildBrocTreeLazy  where
    go :: BTree Data -> SternPath -> [Fraction]
    go (BNode (_, frac, _) _ _) []     = [frac]
    go (BNode (_, frac, _) l r) (p:ps) = frac : go (pick p l r) ps where
        pick p l r = if p == L then l else r

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



