data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (x,y) xs = destinationAux (x,y) North xs

destinationAux :: (Int,Int) -> Direction -> [Command] -> (Int,Int)
destinationAux (x,y) dir [] = (x,y)

destinationAux (x,y) North (TurnLeft:xs) = destinationAux (x,y) West xs
destinationAux (x,y) North (TurnRight:xs) = destinationAux (x,y) East xs
destinationAux (x,y) South (TurnLeft:xs) = destinationAux (x,y) East xs
destinationAux (x,y) South (TurnRight:xs) = destinationAux (x,y) West xs

destinationAux (x,y) East (TurnLeft:xs) = destinationAux (x,y) North xs
destinationAux (x,y) East (TurnRight:xs) = destinationAux (x,y) South xs
destinationAux (x,y) West (TurnLeft:xs) = destinationAux (x,y) South xs
destinationAux (x,y) West (TurnRight:xs) = destinationAux (x,y) North xs

destinationAux (x,y) North ((Forward w):xs) = destinationAux (x, y+w) North xs
destinationAux (x,y) North ((Backward w):xs) = destinationAux (x, y-w) North xs
destinationAux (x,y) South ((Forward w):xs) = destinationAux (x, y-w) South xs
destinationAux (x,y) South ((Backward w):xs) = destinationAux (x, y+w) South xs
destinationAux (x,y) West ((Forward w):xs) = destinationAux (x-w, y) West xs
destinationAux (x,y) West ((Backward w):xs) = destinationAux (x+w, y) West xs
destinationAux (x,y) East ((Forward w):xs) = destinationAux (x+w, y) East xs
destinationAux (x,y) East ((Backward w):xs) = destinationAux (x-w, y) East xs


main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result