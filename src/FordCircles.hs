module FordCircles where
import           Farey
import           Fractions



type FordCircle = (Float, Float, Float)


fordCircles :: [Fraction] ->  [FordCircle]
fordCircles = fmap fordCircle



fordCircle :: Fraction -> FordCircle
fordCircle  (F p 0) = (0, 0, 0)
fordCircle  (F p q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q*q)
scaleFordCircle :: Float ->  FordCircle -> FordCircle
scaleFordCircle s (x, y, z) = (s * x, s * y, s * z)
