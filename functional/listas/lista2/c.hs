data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

a = North
b = [Forward 2, TurnLeft, TurnLeft, Forward 1]

faces :: Direction -> [Command] -> Direction
faces dir [] = dir

faces North (TurnLeft:xs) = faces West xs
faces North (TurnRight:xs) = faces East xs
faces South (TurnLeft:xs) = faces East xs
faces South (TurnRight:xs) = faces West xs

faces East (TurnLeft:xs) = faces North xs
faces East (TurnRight:xs) = faces South xs
faces West (TurnLeft:xs) = faces South xs
faces West (TurnRight:xs) = faces North xs   

faces dir (_:xs) = faces dir xs

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result