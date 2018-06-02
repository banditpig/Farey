import           Control.Applicative
import           Data.Monoid
import           Farey
import           FordCircles
import           Graphics.Gloss

-- f(x,y) = ( -(x2+y2-1)/(x2+y2+1) , 2x/(x2+y2+1) )
planeMap :: [(Float, Float, Float)] -> [(Float, Float, Float)]
planeMap  = fmap f  where
    f (r, x, y) = (r, x', y') where
        x' = (-1)*(x^2 + y^2 - 1)/(x^2 + y^2 + 1)
        y' = 2*x / ( x^2 + y^2 + 1)

circ :: FordCircle -> Picture
circ (r, x, y) =  translate x y $ color white $ circleSolid r

makeCircles :: Picture
makeCircles = Pictures $ fmap (circ . scaleFordCircle 500) (fordCircles 50)

main :: IO ()
main =   display
         FullScreen
         black $
         Pictures [makeCircles, translate 500 0 $ rotate 180 makeCircles]


