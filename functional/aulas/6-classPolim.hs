import Data.Char

--funcao monomorfica
--funciona somente para um tipo de dados especifico
capitalize :: Char -> Char
capitalize ch = chr( ord ch + offset )
    where
        offset = ord 'A' - ord 'a'


--polimorfismo
--funcao possui um tipo generico
zip' :: [t] -> [u] -> [(t,u)]
zip' (a:as) (b:bs) = (a,b) : (zip' as bs)
zip' _ _ = []      

--classes
--coleçao de tipos
allEqual :: Eq t => t -> t -> t -> Bool
allEqual m n p = (m == n) && (n == p)

member :: Eq t => [t] -> t -> Bool
member [] b = False
member (a:as) b = (a == b) || (member as b) 

agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar lista = contRepeat (concat lista)

contRepeat :: Eq t => [t] -> [(t, Int)]
contRepeat [] = []
contRepeat (x:xs) = (x, 1 + length( [a | a <- xs, x == a]) ) : (contRepeat [a | a <- xs, x /= a]) 

-- contRepeat :: Eq t => [t] -> [(t, Int)]
-- contRepeat [] = []
-- contRepeat (x:xs) = (x, contRepeatAux x xs) : (contRepeat [a | a <- xs, x /= a ])

-- contRepeatAux :: Eq x => x -> [x] -> Int
-- contRepeatAux _ [] = 1
-- contRepeatAux y (x:xs) | y == x = 1 + contRepeatAux y xs
--                        | otherwise = contRepeatAux y xs



