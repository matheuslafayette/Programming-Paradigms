isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 y = True
isReplica [] n y = False
isReplica xs 0 y = False
isReplica (x:xs) n y | y == x = isReplica xs (n-1) y
                     | otherwise = False

main = do
    a <- getLine
    b <- getLine
    c <- getChar

    let result = isReplica a (read b) c
    print(result)