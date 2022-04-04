-- (- 1) (2.5) Um dos algoritmos mais simples de compressão de dados sem perda é run-length encoding (RLE),
-- em que sequências de dados com o mesmo valor são armazenados como um contador de repetições seguido do
-- dado. Escreva uma função encode_rle que, dada uma String, retorna uma lista de pares contendo um caractere
-- e o número de vezes que ele se repete de forma seguida na String.

-- exemplo: encode_rle "WWWWWWBWWWXYYZZZ" ---> "6W1B3W1X2Y3Z"

-- encode_rle :: String -> String

-- Pode ser usada uma estrutura de dados intermediaria / auxiliar a critério do aluno.
-- Dica: use a função show para converter um inteiro em String

encode_rle :: String -> String
encode_rle [] = ""
encode_rle (x:xs) = (contTimes (x:xs)) ++ encode_rle (dropWhile' xs x)

contTimes :: String -> String
contTimes (x:xs) = show (auxContTimes (x:xs) x) ++ [x]

auxContTimes :: String -> Char -> Int
auxContTimes [] _ = 0
auxContTimes (x:xs) c | x == c = 1 + auxContTimes xs c
                      | otherwise = 0

dropWhile' :: String -> Char -> String
dropWhile' [] _ = []
dropWhile' (x:xs) c | x == c = dropWhile' xs c
                    | otherwise = (x:xs)

-- (- 2) (2.5) Escreva uma função decode_rle que descomprima uma String codificada com RLE. 

-- exemplo: decode_rle "6W1B3W1X2Y3Z" ---> "WWWWWWBWWWXYYZZZ"

-- Pode ser usada uma estrutura da dados intermediaria / auxiliar a critério do aluno.
-- Assuma que a repetição máxima é de 9 caracteres. 
-- Dica: use a função charToInt abaixo para converter um caractere numérico em um inteiro

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

decode_rle :: String -> String
decode_rle [] = ""
decode_rle (n:c:xs) = repeatCh c (charToInt n) ++ decode_rle xs
    
repeatCh :: Char -> Int -> String
repeatCh _ 0 = ""
repeatCh c n = [c] ++ (repeatCh c (n-1))

-- (- 3) (2.5) Outros algoritmos de compressão utilizam um "dicionário que guarda pares de códigos (inteiro)
-- e Strings, de forma que sempre que a String é reutilizada, se usa apenas o código. Para descompactar, é
-- preciso ter o dicionário e a String compactada. Ilemente uma função que recebe um dicionário e uma String
-- compactada e mostre a String descompactada, isto é, sempre que aparecer um número inteiro, ele deve ser
-- substituído pela palavra no dicionário. Para simplificar o problema, assuma que os códigos tem apenas um
-- dígito.

-- type Dicionario = [(Int, String)]

-- exemplo: 
-- meuDicionario :: Diccionario
-- meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]
-- teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
-- decode meuDicionario teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
-- decode :: Dicionario -> String -> String

type Dicionario = [(Int, String)]
meuDicionario = [(1::Int, "casa"), (3::Int, "cafe"), (4::Int, "teria"), (6::Int, "era"), (7::Int, "uma")]
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"

decode :: Dicionario -> String -> String
decode dict [] = ""
decode dict (x:xs) | x <= '9' && x >= '0' = printDict dict (charToInt x) ++ decode dict xs
                   | otherwise = [x] ++ decode dict xs

printDict :: Dicionario -> Int -> String
printDict [] _ = ""
printDict (x:xs) n | fst x == n = snd x
                   | otherwise = printDict xs n

                
{-
4) (2.5) Ao invés de uma busca linear no dicionário representado como uma lista, ele pode ser representado
por uma árvore binária, acelerando a busca. Implemente a função codeTree abaixo que, ao invés de receber o
dicionário como uma lista, o recebe como uma árvore de busca binária.

type DicionarioT = Tree Int String
data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor)
                      | Leaf

meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))

decodeTree meu meuDicionarioT teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
decodeTree :: DicionarioT -> String -> String
-}

type DicionarioT = Tree Int String

data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor)
                      | Leaf

meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))
                                

decodeT :: DicionarioT -> String -> String
decodeT dict [] = ""
decodeT dict (x:xs) | x <= '9' && x >= '0' = printDictT dict (charToInt x) ++ decodeT dict xs
                    | otherwise = [x] ++ decodeT dict xs

printDictT :: DicionarioT -> Int -> String
printDictT Leaf _ = ""
printDictT (Node k v subtr1 subtr2) n | k == n = v
                                      | n > k = printDictT subtr2 n
                                      | otherwise = printDictT subtr1 n                   