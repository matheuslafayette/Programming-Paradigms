--raiz quadrada

--bottom-up
oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b / (2*a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d-e, d+e)
    where
        d = -b / (2*a)
        e = sqrt(b*b - 4*a*c)/(2*a)

roots :: (Float, Float, Float) -> String
roots (a, b, c) | b*b == 4*a*c = show( oneRoot a b c )
                | b*b > 4*a*c = show f ++ " " ++ show s
                | otherwise = "no roots"
    where
        (f,s) = twoRoots a b c

maiorQue :: Int -> Int -> Int
maiorQue a b | a > b = a
             | otherwise = b

menorQue :: Int -> Int -> Int
menorQue a b | a < b = a
             | otherwise = b

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (x, y)
    where
        x = menorQue (menorQue a b) c
        y = maiorQue (maiorQue a b) c

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = (x, y, z)
    where
        (x, z) = menorMaior a b c
        y | a <= b && a >= c || a <= c && a >= b = a
          | b <= a && b >= c || b <= c && b >= a = b
          | otherwise = c

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

ehVertical :: Reta -> Bool
ehVertical ( (x1, y1), (x2, y2) ) = (x1 == x2) && (y1 /= y2)
        