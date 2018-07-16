{-# LANGUAGE QuasiQuotes #-}
module JScript where
import           BinaryTrees
import           Fractions
import           RationalTrees
import           Text.RawString.QQ
topPage :: String
topPage = [r|
    <html>
    <head>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/gojs/1.8.21/go-debug.js"></script>
      <script id="code">
      function init() {
        var $ = go.GraphObject.make;
        myDiagram = $(go.Diagram, "treeDiv",
        {
          initialAutoScale: go.Diagram.UniformToFill,
          layout: $(go.TreeLayout, { nodeSpacing: 5, layerSpacing: 30 })
        });
        var model = $(go.TreeModel);
        model.nodeDataArray =
        [
 |]

bottomPage :: String
bottomPage = [r|
        ];
        myDiagram.model = model;
      }
    </script>
   </head>
   <body onload="init()">
   <div id="tree">
   <!-- The DIV for the Diagram needs an explicit size or else we won't see anything.
        This also adds a border to help see the edges of the viewport. -->
   <div id="treeDiv" style="border: solid 1px black; width:100%; height:100%"></div>

 </div>
</body>
</html>
|]

fullPage :: (Show a) => BTree a ->  String
fullPage tr = concat [topPage, goJSModel . toNodeParentList $ tr, bottomPage]

-- a given node might/might not have children.
toNodeParentList :: BTree a -> [(a, Maybe a, Maybe a)]
toNodeParentList = foldBTreeNodes f where
        f v Empty          Empty          = (v, Nothing, Nothing)
        f v Empty          (BNode vr _ _) = (v, Nothing, Just vr)
        f v (BNode vl _ _) Empty          = (v, Just vl, Nothing)
        f v (BNode vl _ _) (BNode vr _ _) = (v, Just vl, Just vr)

shw :: (Show a) => a -> String
shw x = "'" ++ show x ++ "'"

goJSModel :: (Show a) => [(a, Maybe a, Maybe a)] -> String
goJSModel xs@((r, _, _):_) = foldr f (concat["{key:", shw r, "}"]) xs  where
    f  (w, Just x,  Just y)  ac =
        concat [ "{key:", shw x, ",parent:", shw w,"},",
        "{key:", shw y, ",parent:", shw w,"},", ac ]
    f  (w, Nothing, Just y)  ac =
        concat ["{key:", shw y, ",parent:", shw w,"},", ac ]
    f  (w, Just x,  Nothing) ac =
        concat [ "{key:", shw x, ",parent:", shw w,"},", "{key:", shw x,"}", ac ]
    f  (w, Nothing, Nothing) ac = ac

makeFractionTreeHTML :: FilePath -> BTree Fraction -> IO ()
makeFractionTreeHTML fname tr = writeFile fname (fullPage tr)




