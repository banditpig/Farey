module Fractions where
import           Data.Monoid
import           Data.Ratio
data Fraction  = F Integer Integer

instance Show Fraction where show (F n d) = show n ++ "/" ++ show d

instance Eq Fraction where
    (==) f1@(F m n) f2@(F p q) = m' == p' && n' == q' where
        F m' n' = reduce f1
        F p' q' = reduce f2

instance Ord Fraction where
    (<=) (F m n ) (F p q) =  m * q <= n * p
    (<)  (F m n ) (F p q) =  m * q < n * p

reduce :: Fraction -> Fraction
reduce (F p q)
    | q == 0 = F p 0
    | q < 0 = reduce ( F (-p) (abs q))
    | otherwise =  F p' q'
        where p' = p `div` gDiv
              q' = q `div` gDiv
              gDiv = gcd p q

instance Monoid Fraction where
    mempty = F 0 0
    mappend (F a b) (F c d) = F (a + c) (b + d)


inv :: Fraction -> Fraction
inv (F p q) = F q p

eval :: Fraction -> Float
eval (F n d) = fromIntegral n / fromIntegral d

denom :: Fraction -> Integer
denom (F _ d) = d

numer :: Fraction -> Integer
numer (F n _) = n

