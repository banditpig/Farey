{-# LANGUAGE QuasiQuotes #-}
module JScript where
import           BinaryTrees
import           Text.RawString.QQ

multiline :: String
multiline = [r|<HTML>|]
-- var model = $(go.TreeModel);
-- model.nodeDataArray =
-- [
--   { key: "A" },
--   { key: "B", parent: "A" },
--   { key: "C", parent: "B" }
-- ];
-- myDiagram.model = model;
-- for every value in the tree identify its parent.

-- data Child a = Child a | None deriving (Show)
-- a given node might/might not have children.
toNodeParentList :: BTree a -> [(a, Maybe a, Maybe a)]
toNodeParentList = foldBTreeNodes f where
        f v Empty           Empty         = (v, Nothing, Nothing)
        f v Empty          (BNode vr _ _) = (v, Nothing, Just vr)
        f v (BNode vl _ _) Empty          = (v, Just vl, Nothing)
        f v (BNode vl _ _) (BNode vr _ _) = (v, Just vl, Just vr)


goJSModel :: (Show a) => [(a, Maybe a, Maybe a)] -> String
goJSModel  = foldr f ""  where
    f  (w, Just x,  Just y)  ac =
        concat [ "{key:", show w, ",parent:", show x,"},","{key:", show w, ",parent:", show y,"},", ac ]
    f  (w, Nothing, Just y)  ac =
        concat ["{key:", show w, ",parent:", show y,"},", ac ]
    f  (w, Just x,  Nothing) ac =
        concat [ "{key:", show w, ",parent:", show x,"},", ac ]
    f  (w, Nothing, Nothing) ac =
        concat [ "{key:", show w, "},", ac ]




