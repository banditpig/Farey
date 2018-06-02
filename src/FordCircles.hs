module FordCircles where
import           Farey
import           Fractions



type FordCircle = (Float, Float, Float)

-- for Farey of order n
fordCircles :: Integer -> [FordCircle]
fordCircles = fmap fordCircle . farey

fordCircle :: Fraction -> FordCircle
fordCircle  (F p q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q*q)
scaleFordCircle :: Float ->  FordCircle -> FordCircle
scaleFordCircle s (x, y, z) = (s * x, s * y, s * z)
