data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node v subtr1 subtr2) = v >= (fold ls max) && v <= (fold rs min) && isBST subtr1 && isBST subtr2
    where
        ls = v:(collapse subtr1)
        rs = v:(collapse subtr2)

collapse :: Tree t -> [t]
collapse Nilt = []
collapse (Node value subtree1 subtree2) = value : (collapse subtree1 ++ collapse subtree2) 

fold :: Ord t => [t] -> (t -> t -> t) -> t
fold xs f = foldr1 f xs
