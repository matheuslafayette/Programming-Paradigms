data Estacao = Inverno | Outono | Primavera | Verao
    deriving Show

data Temp = Quente | Frio
    deriving Show

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

type Nome = String
type Idade = Int

data Pessoas = Pessoa Nome Idade
    deriving Show

showPerson :: Pessoas -> String
showPerson (Pessoa n i) = n ++ "--" ++ show i

data Shape = Circle Float | Rectangle Float Float | Square Float
    deriving Show  
    
isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Square _) = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
area (Square a) = a * a

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
    deriving Show

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

testeExpr = Add (Add (Lit 5) (Lit 9)) (Sub (Lit 4) (Lit 2))

showExpr :: Expr -> String
showExpr (Lit e) = show e
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"

data List t = Nil | Cons t (List t)
    deriving Show

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

data Tree t = Leaf | Node t (Tree t) (Tree t)
    deriving Show

testTree = Node 5 (Node 6 Leaf (Node 3 Leaf Leaf))
                   (Node 2 (Node 7 Leaf Leaf) (Node 88 Leaf (Node 3 Leaf Leaf)))
            
depth :: Tree t -> Int
depth Leaf = 0
depth (Node value subtree1 subtree2) = 1 + max (depth subtree1) (depth subtree2)

collapse :: Tree t -> [t]
collapse Leaf = []
collapse (Node value subtree1 subtree2) = value : (collapse subtree1 ++ collapse subtree2)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f Leaf = Leaf
mapTree f (Node value subtree1 subtree2) = Node (f value) (mapTree f subtree1) (mapTree f subtree2) 

data Tree2 t = Leaf2 t | Node2 (Tree2 t) (Tree2 t)
    deriving Show

testTree2 = Node2 (Node2 (Leaf2 3) (Leaf2 55))
                  (Node2 (Node2 (Leaf2 33) (Node2 (Leaf2 22) (Leaf2 21))) (Leaf2 44))

depth2 :: Tree2 t -> Int
depth2 (Leaf2 t) = 1
depth2 (Node2 subtr1 subtr2) = 1 + max (depth2 subtr1) (depth2 subtr2)

collapse2 :: Tree2 t -> [t]
collapse2 (Leaf2 t) = [t]
collapse2 (Node2 subtr1 subtr2) = collapse2 subtr1 ++ collapse2 subtr2

mapTree2 :: (t -> u) -> Tree2 t -> Tree2 u
mapTree2 f (Leaf2 t) = Leaf2 (f t)
mapTree2 f (Node2 subtr1 subtr2) = Node2 (mapTree2 f subtr1) (mapTree2 f subtr2)