module Farey where
import           BinaryTrees
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Fractions

data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]


dropFirstLast :: [a] -> [a]
dropFirstLast xs@(_:_) = tail (init xs)
dropFirstLast _        = []

coPrimes :: Integer -> [Fraction]
coPrimes n = nub [ reduce (F p q) | p <- [-n..n], q <- [-n..n]]
-- Using recurrence relation
farey :: Integer -> [Fraction]
farey 1 = [F 0 1, F 1 1]
farey n = farey' (F 0 1) (F 1 n) where
    farey' (F a b) (F c d)
        | p == 1 && q == 1 = [F a b, F c d, F 1 1]
        | otherwise = F a b : farey' (F c d) (F p q) where
            n' = floor $ fromIntegral (n + b) / fromIntegral d
            p = n' * c - a
            q = n' * d - b

farey1 :: Integer -> [Fraction]
farey1 n = F 0 1 :  mid ++  [F 1 1] where
    mid = nub . sort $ [ reduce ( F p q) | p <- [1..n], q <- [1..n], p < q]

-- Recursive insertion of terms up to given order
fareyForOrder :: Integer -> [Fraction]
fareyForOrder 0 = [F 0 1, F 1 1]
fareyForOrder order = process (fareyForOrder (order - 1)) where
    process [f] = [f]
    process (F m n :F m' n':fs) = if n + n' == order
                                                then
                                                    F m n : F (m + m') order : process (F m' n' : fs)
                                                else
                                                    F m n : process (F m' n':fs)
maxDenom :: [Fraction] -> Integer
maxDenom  = foldr f 1 where f (F _ q) = max q

-- produce next level given the current level.
-- For input list the length is n then a(n) = a(n-1) + phi(n) with a(0) = 1
-- see https://oeis.org/A005728
fareyFromPreviousSeq :: [Fraction] -> [Fraction]
fareyFromPreviousSeq fs = proc fs (1 + maxDenom fs)  where
    proc [f] _ = [f]
    proc (F m n :F m' n':fs) l = if n + n' == l
                                    then
                                        F m n : F (m + m') l : proc (F m' n': fs) l
                                    else
                                        F m n : proc (F m' n':fs) l



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

