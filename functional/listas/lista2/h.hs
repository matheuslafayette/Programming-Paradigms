data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt a) = a
evalTree (Node SUM tree1 tree2) = evalTree tree1 + evalTree tree2
evalTree (Node MUL tree1 tree2) = evalTree tree1 * evalTree tree2
evalTree (Node SUB tree1 tree2) = evalTree tree1 - evalTree tree2

main = do
    s <- getLine
    let result = evalTree (read s)
    print result
