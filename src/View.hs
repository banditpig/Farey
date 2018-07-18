import           BinaryTrees
import           Data.Monoid
import           Farey
import           Graphics.Gloss
import           RationalTrees



makePics :: (Show a) => Float -> Float -> BTree (a, Pos) -> [Picture]
makePics scaleX scaleY = foldBTreeNodes f where
    -- f :: (a -> BTree a -> BTree a -> b)
    rndr t =  Circle 30 <> Translate 0 35 (Scale 0.2 0.2 (Text t))
    sx xx = scaleX * fromIntegral xx
    sy yy =  -scaleY * fromIntegral yy
    f (v, (x,y)) Empty  Empty =
        Translate x' y' (rndr (show v)) where
            x' = sx x
            y' = sy y
    f (v, (x,y)) Empty  (BNode (_, (rx,ry)) _ _) =
        lineR <> Translate x' y'  (rndr (show v))  where
            x' = sx x
            y' = sy y
            rx' = sx rx
            ry' =  sy ry
            lineR = Line [(x', y'), (rx', ry')]
    f (v, (x,y)) (BNode (_, (lx,ly)) _ _)  Empty =
        lineL <>  Translate x' y'  (rndr (show v))  where
            x' = sx x
            y' = sy y
            lx' = sx lx
            ly' =  sy ly
            lineL = Line [(x', y'), (lx', ly')]
    f (v, (x,y)) (BNode (_, (lx,ly)) _ _)  (BNode (_, (rx,ry)) _ _) =
        lineL <> lineR <> Translate x' y' (rndr (show v)) where
            x' = sx x
            y' = sy y
            lx' = sx lx
            ly' = sy ly
            lineL = Line [(x', y'), (lx', ly')]

            rx' = sx rx
            ry' =  sy ry
            lineR = Line [(x', y'), (rx', ry')]
window :: Display
window = InWindow "Window" (1400, 800) (0, 0)

background :: Color
background = greyN 0.7

drawPics :: Picture -> IO ()
drawPics  = display window background

renderTree :: (Show a) => BTree a -> IO ()
renderTree  = drawPics . mconcat . makePics 75 75 . layoutCompact

main :: IO ()
main = renderTree . buildBrocTree $ 10
-- main = renderTree . treeFromList $ [1..500]
-- λ-> take 20 . fmap numer .  fractionPath . interleave (repeat L) $ (repeat R) -- FIB!
