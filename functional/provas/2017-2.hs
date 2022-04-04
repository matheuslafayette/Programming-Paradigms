{-
1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
A primeira posição na lista tem índice 0 (zero).
Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
Exemplos: locate 'x' "abcdewxyz" ------>  6
          locate 5   [5,98,7,32] ------>  0
          locate True [False, False] --> -1
locate :: Eq t => t -> [t] -> Int
-}

locate :: Eq t => t -> [t] -> Int
locate n xs = locateAux n xs 0

locateAux :: Eq t => t -> [t] -> Int -> Int
locateAux _ [] _ = -1
locateAux n (x:xs) i | n == x = i
                     | otherwise = locateAux n xs (i+1)


{-
2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String ésubstring de outra).
Exemplos: substr "abc" "xyz12abrt" ----> False
          substr "abc" "aaabrsabcfr" --> True
          substr "aab" "aacrtxxeaayb" -> False
substr :: String -> String -> Bool
-}

substr :: String -> String -> Bool
substr _ [] = False
substr xs (y:ys) = substrAux xs (y:ys) || substr xs ys

substrAux :: String -> String -> Bool
substrAux [] _ = True
substrAux _ [] = False
substrAux [x] (y:ys) = x == y
substrAux (x:xs) (y:ys) | x == y = substrAux xs ys
                        | otherwise = False

{-
3) Um robô é controlado por 4 comandos: 
   Left, para girar sua direção à esquerda 90 graus;
   Right, para girar sua direção à direita em 90 graus;
   Forward seguido de um número N, que indica um avanço de N metros.
   Backward seguido de um número N, que indica um retrocesso de N metros.

Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
(3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

Exemplo de posições/coordenadas:
(-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
(-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
(-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
(-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
(-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
  deriving (Eq, Show)
data Direction = North | South | West | East

exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
         destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
destination :: (Int,Int) -> [Command] -> (Int,Int)
-}        

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
    deriving (Eq, Show)

data Direction = North | South | West | East
    deriving (Show)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (x, y) com = destinationAux (x, y) com North

destinationAux :: (Int,Int) -> [Command] -> Direction -> (Int,Int)
destinationAux (x, y) [] _ = (x, y)

destinationAux (x, y) ((TurnLeft):xs) North = destinationAux (x, y) xs West
destinationAux (x, y) ((TurnLeft):xs) West = destinationAux (x, y) xs South
destinationAux (x, y) ((TurnLeft):xs) South = destinationAux (x, y) xs East
destinationAux (x, y) ((TurnLeft):xs) East = destinationAux (x, y) xs North

destinationAux (x, y) ((TurnRight):xs) North = destinationAux (x, y) xs East
destinationAux (x, y) ((TurnRight):xs) East = destinationAux (x, y) xs South
destinationAux (x, y) ((TurnRight):xs) South = destinationAux (x, y) xs West
destinationAux (x, y) ((TurnRight):xs) West = destinationAux (x, y) xs North

destinationAux (x, y) ((Forward n):xs) North = destinationAux (x, y+n) xs North
destinationAux (x, y) ((Forward n):xs) South = destinationAux (x, y-n) xs South
destinationAux (x, y) ((Backward n):xs) North = destinationAux (x, y-n) xs North
destinationAux (x, y) ((Backward n):xs) South = destinationAux (x, y+n) xs South

destinationAux (x, y) ((Forward n):xs) East = destinationAux (x+n, y) xs East
destinationAux (x, y) ((Forward n):xs) West = destinationAux (x-n, y) xs West
destinationAux (x, y) ((Backward n):xs) East = destinationAux (x-n, y) xs East
destinationAux (x, y) ((Backward n):xs) West = destinationAux (x+n, y) xs West

test1 = destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1]
test2 = destination (0,0) [Backward 2, Forward 1]

{-
4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos (North, South, East ou West), assumindo que ele começa voltado para a direção North.
exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
         faces North [Backward 2, Forward 1] ---> North
         faces North [TurnLeft, TurnLeft, TurnLeft] ---> East
faces ::  Direction -> [Command] -> Direction
-}

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir ((Forward n): xs) = faces dir xs
faces dir ((Backward n): xs) = faces dir xs

faces North ((TurnLeft):xs) = faces West xs
faces West ((TurnLeft):xs) = faces South xs
faces South ((TurnLeft):xs) = faces East xs
faces East ((TurnLeft):xs) = faces North xs

faces North ((TurnRight):xs) = faces East xs
faces East ((TurnRight):xs) = faces South xs
faces South ((TurnRight):xs) = faces West xs
faces West ((TurnRight):xs) = faces North xs

t1 = faces North [Forward 2, TurnLeft, TurnLeft, Forward 1]-- ---> South
t2 = faces North [Backward 2, Forward 1]-- ---> North
t3 = faces North [TurnLeft, TurnLeft, TurnLeft]-- ---> East