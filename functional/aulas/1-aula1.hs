answer :: Int 
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int 
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual m n p = (m == n) && (n == p)

maxi :: Int -> Int -> Int 
maxi n m | n >= m = n 
         | otherwise = m

imc :: Float -> Float -> Float
imc p a = p / (a * a)

greater3 :: Int -> Int -> Int -> Bool
greater3 a b c = (a < b) && (b < c)

--parametro: semana
--saida: vendas na semana
vendas :: Int -> Int
vendas n = mod n 6

totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0
              | n > 0 = vendas n + totalVendas (n-1)

vendasIguais :: Int -> Int -> Int
vendasIguais n s | s == vendas n = 1
                 | otherwise = 0

totalVendasIguais :: Int -> Int -> Int
totalVendasIguais n s | n == 0 = vendasIguais 0 s
                      | n > 0 = vendasIguais n s + totalVendasIguais (n-1) s

primo :: Int -> Bool
primo n = verifPrimo n (div n 2)
 where verifPrimo :: Int -> Int -> Bool
       verifPrimo n i | i <= 1 = True
                      | mod n i == 0 = False
                      | otherwise = verifPrimo n (i-1)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi m n = verifPrimosEntreSi m n (min m n)

verifPrimosEntreSi :: Int -> Int -> Int -> Bool
verifPrimosEntreSi m n i | i == 1 = True
                         | mod m i == 0 && mod n i == 0 = False
                         | otherwise = verifPrimosEntreSi m n (i-1)

fat :: Int -> Int
fat n | n <= 1 = 1
      | otherwise = n * fat (n-1)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = allEqual a b c && allEqual b c d

equal :: Int -> Int -> Int
equal a b | a == b = 1
          | otherwise = 0

equalCount :: Int -> Int -> Int -> Int
equalCount a b c | a == b && b == c = 3
                 | a /= b && b /= c = 0
                 | otherwise = 2