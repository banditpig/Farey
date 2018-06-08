module FordCircles where
import           Farey
import           Fractions



type FordCircle = (Float, Float, Float)

-- for Farey of order n
fordCircles :: (Integer -> [Fraction]) -> Integer ->  [FordCircle]
fordCircles f = fmap fordCircle . f



fordCircle :: Fraction -> FordCircle
fordCircle  (F p 0) = (0, 0, 0)
fordCircle  (F p q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q^2)
scaleFordCircle :: Float ->  FordCircle -> FordCircle
scaleFordCircle s (x, y, z) = (s * x, s * y, s * z)
