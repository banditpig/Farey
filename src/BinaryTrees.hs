module BinaryTrees where

data BTree a = Empty | BNode a (BTree a) (BTree a)

instance Functor BTree where
    fmap _ Empty                  = Empty
    fmap f (BNode v Empty Empty ) = BNode (f v) Empty Empty
    fmap f (BNode v l r )         = BNode (f v) (fmap f l) (fmap f r)


foldTree :: (a -> b) ->  BTree a -> [b]
foldTree f Empty         = []
foldTree f (BNode v l r) = f v : foldTree f l ++ foldTree f r

foldTreeNodes :: (a -> BTree a -> BTree a -> b) ->  BTree a -> [b]
foldTreeNodes f Empty         = []
foldTreeNodes f (BNode v l r) = f v l r : foldTreeNodes f l ++ foldTreeNodes f r


traverseDepthFirst :: BTree a -> [a]
traverseDepthFirst Empty        = []
traverseDepthFirst (BNode a l r) = a : traverseDepthFirst l ++ traverseDepthFirst r

traverseBreadthFirst :: BTree a -> [a]
traverseBreadthFirst tree = go [tree]
    where
        go [] = []
        go xs = fmap bNodeVal xs ++ go (concatMap lrSubTrees xs)
        bNodeVal   (BNode v _ _)         = v
        lrSubTrees (BNode _ Empty Empty) = []
        lrSubTrees (BNode _ Empty b)     = [b]
        lrSubTrees (BNode _ a Empty)     = [a]
        lrSubTrees (BNode _ a b)         = [a,b]


traverseInOrder :: BTree a -> [a]
traverseInOrder Empty         = []
traverseInOrder (BNode v l r) = traverseInOrder l ++ [v] ++ traverseInOrder r

traversePreOrder :: BTree a -> [a]
traversePreOrder Empty         = []
traversePreOrder (BNode v l r) = v : traversePreOrder l ++ traversePreOrder r

depth :: BTree a -> Int
depth Empty         = 0
depth (BNode _ l r) = 1 +  max (depth l) (depth r)

width :: BTree a -> Int
width t = leftCount t + rightCount t where
    leftCount t = goLeft t 1 where
        goLeft (BNode _ Empty _) n = n
        goLeft (BNode _ l     _) n = goLeft l (n + 1)
    rightCount t = goRight t 1 where
        goRight (BNode _ _ Empty) n = n
        goRight (BNode _ _ r    ) n = goRight r (n + 1)


treeFromList :: (Ord a) => [a] -> BTree a
treeFromList [] = Empty
treeFromList (x:xs) = BNode x (treeFromList (filter (<x) xs))
                              (treeFromList (filter (>=x) xs))

printTree :: (Show a) => BTree a -> IO ()
printTree t = putStrLn $ prettyTree t

prettyTree :: Show a => BTree a -> String
prettyTree = unlines . layoutTree where
    indent = fmap ("  "++)
    layoutTree Empty = []
    layoutTree (BNode v left  right) = indent (layoutTree right) ++ [show v] ++ indent (layoutTree left)
-- In this layout strategy, the position of a node v is obtained by the following two rules:

-- x(v) is equal to the position of the node v in the inorder sequence
-- y(v) is equal to the depth of the node v in the tree
type Pos = (Int, Int)
layout :: BTree a -> BTree (a, Pos )
-- layout :: Tree a -> Tree (a, Pos)
layout t = fst (go 1 1 t)
  where go x y Empty = (Empty, x)
        go x y (BNode a l r) = (BNode (a, (x',y)) l' r', x'')
          where (l', x')  = go x (y+1) l
                (r', x'') = go (x'+1) (y+1) r

layoutCompact :: BTree a -> BTree (a, Pos)
layoutCompact t = t'
  where (l, t', r) = layoutAux x1 1 t
        x1 = maximum l + 1

        layoutAux :: Int -> Int -> BTree a -> ([Int], BTree (a, Pos), [Int])
        layoutAux x y Empty = ([], Empty, [])
        layoutAux x y (BNode a l r) = (ll', BNode (a, (x,y)) l' r', rr')
          where (ll, l', lr) = layoutAux (x-sep) (y+1) l
                (rl, r', rr) = layoutAux (x+sep) (y+1) r
                sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
                ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
                rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)
                -- overlay xs ys = xs padded out to at least the length of ys
                -- using any extra elements of ys
                overlay :: [a] -> [a] -> [a]
                overlay [] ys         = ys
                overlay xs []         = xs
                overlay (x:xs) (y:ys) = x : overlay xs ys

tree64 :: BTree Char
tree64 = BNode 'n'
                (BNode 'k'
                        (BNode 'c'
                                (BNode 'a' Empty Empty)
                                (BNode 'h'
                                        (BNode 'g'
                                                (BNode 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (BNode 'm' Empty Empty)
                )
                (BNode 'u'
                        (BNode 'p'
                                Empty
                                (BNode 's'
                                        (BNode 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )



