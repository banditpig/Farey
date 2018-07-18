module Matrix where
import           BinaryTrees
import           Control.Monad
import           Data.Monoid
import           Fractions
import           RationalTrees


data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]


data Matrix = M  Integer Integer Integer Integer deriving Show

instance Monoid Matrix where
    mempty = i -- Identity 2x2 matrix
    mappend (M a b c d) (M w x y z) =
        M (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

l :: Matrix
l = M 1 1 0 1

r :: Matrix
r = M 1 0 1 1

i :: Matrix
i = M 1 0 0 1

frac1, frac2, frac3 :: Matrix -> Fraction
frac1 (M a b c d) = F c a
frac2 (M a b c d) = F b d
frac3 (M a b c d) = F (c + d) (a + b)

sternTermMatrix :: SternTerm -> Matrix
sternTermMatrix t = if t == L then l else r

reduceMatrix :: Matrix -> Fraction
reduceMatrix (M a b c d) = F (c + d) (a + b)

reduceSternPath :: SternPath -> Fraction
reduceSternPath pth  = reduceMatrix $ foldl (\ acc t -> sternTermMatrix t <> acc )  i pth

--reduceSternPath' :: SternPath -> Fraction
reduceSternPath' = mconcat . fmap sternTermMatrix


sternPath :: Fraction -> SternPath
sternPath frac = sPath where
    sPath = go (fraction <$> buildBrocTreeLazy) fr  [] where
        fr = reduce frac
        go (BNode (F p q) l r) (F n d) path
            | p == n && q == d   = path
            | F p q < F n d      = go r fr (R : path)
            | otherwise          = go l fr (L : path)



sternPathnm :: Fraction -> SternPath
sternPathnm (F m n) = go m n  where
    go m' n'
        | n' == m' = []
        | m' < n'   = L : go m' (n' - m')
        | otherwise = R : go (m' - n') n'

sternPathFloat :: Float -> SternPath
sternPathFloat  = go  where
    go d
        | d < 1 = L : go (d / (1 - d))
        | otherwise = R : go (d - 1)

fractionPathString :: String -> [Fraction]
fractionPathString  = fractionPath . fmap (\x -> if x == 'L' then L else R)


fractionPath :: SternPath -> [Fraction]
fractionPath  = go ( fraction <$> buildBrocTreeLazy)  where
    go (BNode frac _ _) []     = [frac]
    go (BNode frac l r) (p:ps) = frac : go (pick p l r) ps where
        pick p l r = if p == L then l else r

