module Rats where
import           Control.Applicative
import           Data.List
import           Data.Ratio
import           Debug.Trace         (traceShow)
import           Fractions
traceShow' arg = traceShow arg arg

toFraction :: Ratio Integer -> Fraction
toFraction r = F (numerator r) (denominator r)

ratsP :: [Fraction]
ratsP = fmap toFraction $ iterate next 1 where
    next x = recip (fromInteger n + 1 - y) where (n,y) = properFraction x

-- rats8 :: [Rational]
rats :: [Fraction]
rats = fmap toFraction  $ iterate next'  0
    where
        next' 0 = 1
        next'  x | x > 0 = negate x
           | otherwise = next (negate x)
        next x = recip (fromInteger n + 1 - y) where (n,y) = properFraction x

stern1 :: Integer -> [Integer]
stern1 n = reverse ns where
    (_, _, ns)   = head . dropWhile f . iterate update $ (0, 1, [1,1])
    f (_, _, xs) = (toInteger . length $ xs) <= n
    update (ia, ib, xs) = (ia + 1, ib + 1, b:a + b:xs) where
        len = length xs - 1
        a = xs !! (len - ia)
        b = xs !! (len - ib)

-- 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7, 2, 7

-- Stern's diatomic series (or Stern-Brocot sequence):
--     a(0) = 0,
--     a(1) = 1;
--     for n > 0: a(2*n) = a(n),
--     a(2*n+1) = a(n) + a(n+1).
stern2 :: [Integer]
stern2 = [next n | n <- [1..]] where
    next :: Integer -> Integer
    next n
        | n < 2 = n
        | even n = next (n `div` 2)
        | otherwise = next n'   + next (n' + 1) where
            n' = (n - 1) `div` 2

-- 0, 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7
stern3 :: [Integer]
stern3 = 1 : 1 : knit (tail stern3) stern3
  where
    knit (a:as) (b:bs) = a + b : a : knit as bs


fracs1 :: [Integer] -> [Fraction]
fracs1 []       = []
fracs1 [p,q]    = [F p q]
fracs1 (p:q:ns) = F p q : fracs1 (q:ns)



-- [1/1,1/2,2/1,1/3,3/2,2/3,3/1,1/4,4/3,3/5,5/2,2/5,5/3,3/4,4/1,1/5,5/4,4/7,7/3,3/8]
fracs2 :: [Integer] -> [Fraction]
fracs2 xs = fromTuple <$> (zip <*> tail $ xs) where fromTuple (p, q) = F p q

--1. fmap fromTuple $ zip xs (tail xs)
--2. fromTuple <$> zip xs (tail xs)
--3.
