data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Show)

a = Node 2 (Node 3 Nilt Nilt) (Nilt)

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList tree [] = tree
insertList (Node v subtr1 subtr2) (x:xs) = (Node v subtr1 adnode)
    where
        adnode = Node x Nilt Nilt

