import           Control.Applicative
import           Data.Complex
import           Data.Monoid
import           Debug.Trace                          (traceShow)
import           Farey
import           FordCircles
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Simulate
type C = Complex Float
traceShow' arg = traceShow arg arg

i :: C
i = 0 :+ 1

fz z = (z - i)/(z + i)

planeMap :: [FordCircle] -> [FordCircle]
planeMap  = fmap f  where
    f (r, x, y) = (newRad, x', y') where
        z = x :+ y
        z1 = (x + r) :+ y
        newCenter = fz z
        newRad = magnitude (newCenter - fz z1)
        x' = realPart newCenter
        y' = imagPart newCenter



-- circ :: FordCircle -> Picture
-- circ (r, x, y) =  translate x y $ color white $ circleSolid r
newColour :: FordCircle -> Color
newColour (r, x, y) = makeColor  0.6 0.2 0.6 1.0



circ :: FordCircle -> Picture
circ c@(r, x, y) =  translate x y $ color (newColour c)  $ Circle r


makeCircles' :: Integer -> ([FordCircle] -> [FordCircle]) ->  Picture
makeCircles' 0 _ = Blank
makeCircles' n f = Pictures $ fmap (circ . scaleFordCircle 100) (f $ fordCircles farey n)

makeCircles :: ([FordCircle] -> [FordCircle]) ->  Picture
makeCircles f = Pictures $ fmap (circ . scaleFordCircle 100) (f $ fordCircles coPrimes 50)

main :: IO ()
main = display
         (InWindow "Window" (1400, 800) (0, 0))
         (greyN 0.1)
         (Pictures [makeCircles planeMap, translate 0 (-250)  $ makeCircles id ])
frame cs t = Pictures $  fmap (circ . scaleFordCircle 250) (take n $ cs)  where
    n = round (t * 1000) `mod` length cs



-- main :: IO ()
-- main = do
--           let mappedCircs = planeMap $ fmap fordCircle (coPrimes 50)
--           animate (InWindow "Farey" (1400, 800) (20, 20))
--                 (greyN 0.1) (frame mappedCircs)

