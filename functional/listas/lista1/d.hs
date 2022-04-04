decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] _ = []
decEnigma (x:xs) ys = (decEnigmaAux x ys) : (decEnigma xs ys)

decEnigmaAux :: Char -> [(Char, Char)] -> Char
decEnigmaAux x (y:ys) | x == fst(y) = snd(y)
                      | otherwise = decEnigmaAux x ys