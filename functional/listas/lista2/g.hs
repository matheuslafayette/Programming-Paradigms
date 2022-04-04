data Tree t = Node t (Tree t) (Tree t) | Nilt
    deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node v subtr1 subtr2) = 1 + max (alturaArvore subtr1) (alturaArvore subtr2)

main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result