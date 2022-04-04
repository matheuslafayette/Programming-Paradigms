stringToInt :: String -> Int
stringToInt x = read x

btoi :: String -> Int
btoi x = btoiAux x 0

btoiAux :: String -> Int -> Int
btoiAux [] _  = 0
btoiAux (x:xs) sum = sum + (stringToInt [x]) * 2^(length xs) + btoiAux xs sum

main = do
    s <- getLine
    let result = btoi s
    print result