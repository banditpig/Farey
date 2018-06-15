module Farey where
import           Data.List
import           Fractions

data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]


fractionsN :: Integer -> [Fraction]
fractionsN n = nub  [ reduce (F p q) | p <- [-n..n], q <- [-n..n]]
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


