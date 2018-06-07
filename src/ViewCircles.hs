import           Control.Applicative
import           Data.Complex

import           Data.Monoid
import           Farey
import           FordCircles
import           Graphics.Gloss
type C = Complex Float

i :: C
i = 0 :+ 1

fz z = (z - i)/(z + i)

planeMap :: [FordCircle] -> [FordCircle]
planeMap  = fmap f  where
    f (r, x, y) = (newRad, x', y') where
        z = x :+ y
        z1 = (x + r) :+ (y + r)
        newCenter = fz z
        newRad = magnitude (newCenter - fz z1)
        x' = realPart newCenter
        y' = imagPart newCenter




-- circ :: FordCircle -> Picture
-- circ (r, x, y) =  translate x y $ color white $ circleSolid r

circ :: FordCircle -> Picture
circ (r, x, y) =  translate x y $ color red $ circle r


makeCircles :: ([FordCircle] -> [FordCircle]) ->  Picture
makeCircles f = Pictures $ fmap (circ . scaleFordCircle 100) (f $ fordCircles coPrimes 50)

main :: IO ()
main = display
         (InWindow "Window" (1400, 800) (0, 0))
         white
         (Pictures [makeCircles planeMap, translate 0 (-250) $ makeCircles id ])


