import           Control.Applicative
import           Data.Monoid
import           Farey
import           Graphics.Gloss



 -- radius 1/(2q2) and centre at (p/q, 1/(2q2))

-- f(x,y) = ( -(x2+y2-1)/(x2+y2+1) , 2x/(x2+y2+1) )
planeMap :: [(Float, Float, Float)] -> [(Float, Float, Float)]
planeMap  = fmap f  where
    f (r, x, y) = (r, x', y') where
        x' = (-1)*(x^2 + y^2 - 1)/(x^2 + y^2 + 1)
        y' = 2*x / ( x^2 + y^2 + 1)

circ :: (Float, Float, Float) -> Picture
circ (r, x, y) =  translate x y $ color white $ circleSolid r

makeCircles :: Picture
makeCircles = Pictures $ map  circ (fordCirclesScaled 300.0 50)
main =   display
         FullScreen
         black $
         Pictures [rotate 0 makeCircles,
                   translate 300 0 $ rotate 180 makeCircles]


