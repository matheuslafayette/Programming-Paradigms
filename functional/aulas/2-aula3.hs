import Data.Char

--putStr( "abcd\n" ++ "efgh\n" )

upperCase :: Char -> Char
upperCase ch | ch >= 'a' && ch <= 'z' = chr( ord ch + offset)
             | otherwise = ch
 where offset = ord 'A' - ord 'a'

addEspacos :: Int -> String
addEspacos 0 = "" 
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho ++ imprimeSemanas n ++ imprimeTotal n ++ imprimeMedia n
 where
    cabecalho = "Semana   Vendas\n"

    imprimeTotal :: Int -> String
    imprimeTotal n = "Total     " ++ show(totalVendas n) ++ "\n"

    imprimeMedia :: Int -> String
    imprimeMedia n = "Media     " ++ show(mediaVendas n) ++ "\n"

    imprimeSemanas :: Int -> String
    imprimeSemanas 0 = "   " ++ "0" ++ "      " ++ show(vendas 0) ++ "\n"
    imprimeSemanas n = imprimeSemanas (n-1) ++ "   " ++ show n ++ "      " ++ show (vendas n) ++ "\n"
        

vendas :: Int -> Int
vendas n = mod n 3

totalVendas :: Int -> Int
totalVendas n | n == 0 = vendas 0
              | n > 0 = vendas n + totalVendas (n-1)

mediaVendas :: Int -> Float 
mediaVendas n = fromIntegral(totalVendas n) / fromIntegral n

