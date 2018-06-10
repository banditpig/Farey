import           Control.Applicative
import           Control.Monad
import           Data.Complex
import           Data.Monoid
import           Farey
import           FordCircles
import           Fractions
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.IO.Simulate
import           Rats

type Cmplx = Complex Float

i :: Cmplx
i = 0 :+ 1

fz :: Cmplx -> Cmplx
fz z = (z - i)/(z + i)


fzz :: Float -> Cmplx -> Cmplx
fzz flt z=  (z - i)/(z + i)**(flt :+ 0)

makeCircle :: Cmplx -> Cmplx -> Cmplx -> FordCircle
makeCircle z1 z2 z3 = (r, x, y) where
    x1 = realPart z1
    x2 = realPart z2
    x3 = realPart z3
    y1 = imagPart z1
    y2 = imagPart z2
    y3 = imagPart z3
    k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
    x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
    y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
    r = sqrt ((x - x1)^2 + (y - y1)^2)

planeMap :: [FordCircle] -> [FordCircle]
planeMap  = fmap f  where
    f (r, x, y) = makeCircle (fz z1) (fz z2) (fz z3) where
        z1 = (x + r) :+ y
        z2 = (x - r) :+ y
        z3 =  x :+ (y + r)

planeMap' :: Float -> [FordCircle] -> [FordCircle]
planeMap' fl = fmap f where
    f (r, x, y) = makeCircle (fzz fl z1) (fzz fl z2) (fzz fl z3) where
        z1 = (x + r) :+ y
        z2 = (x - r) :+ y
        z3 =  x :+ (y + r)

newColour :: FordCircle -> Color
newColour (r, x, y) = makeColor  (r*5.0) 0.3 0.6 1.0


circ :: FordCircle -> Picture
circ c@(r, x, y) =  translate x y . color (newColour c) . Circle $ r

makeCircles :: ([FordCircle] -> [FordCircle]) -> [Fraction] ->   Picture
makeCircles f =   Pictures . fmap circ . f . fordCircles




frame :: [Fraction] -> Float -> Picture
frame cs fl = rotate (fl*50) $ scale 250 250 $ makeCircles (planeMap' step) cs  where
  step = fl * 0.1
    --  if fl * 0.1 >= 1.0
    --   then 1.0
    --   else fl * 0.1


-- main :: IO ()
-- main = do
--       let cs =  fractionsN  20
--       display
--          (InWindow "Window" (1400, 800) (0, 0))
--          (greyN 0.2)
--          (Pictures [scale 100 100 $ makeCircles planeMap cs, translate 0 (-250) $ scale 100 100 $ makeCircles  id cs])


main :: IO ()
main = do
      let cs = fractionsN 20 -- [Fraction]
      animate
         FullScreen -- (InWindow "Window" (1400, 800) (0, 0))
         (greyN 0.2)
         $ frame cs
