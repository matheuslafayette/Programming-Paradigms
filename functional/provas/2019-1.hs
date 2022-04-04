{-
1) ( 2.5) Escreva uma função que funcione como uma função "meio" ou "mid" do Excel: ela recebe 3 parâmetros, uma String e dois números ( digamos x e y), e retorna a substring que começa no primeiro índice ( o primeiro número, x )e contém y caracteres. A primeira posição na lista tem índice 1 ( um). Casos inválidos retornam a lista vazia (por exemplo, números negativos ou inicio fora do tamanho da lista). Se a lista contiver menos caracteres que o solicitado deve ser retornado o que for possível.
Exemplos: meio "ver isso" 5 4 --------------> "isso"
          meio "ver isso tambem" 1 3 -------> "ver"
          meio "ver isso tambem" 13 10 -----> "bem"
          meio "ver isso tambem" 130 ( -4 )-> ""
meio :: String -> Int -> Int -> String
-}

meio :: String -> Int -> Int -> String
meio str begin end | begin < 0 || begin > (length str) || end < 0 = ""
                   | otherwise = meioAux (drop (begin-1) str) end

meioAux :: String -> Int -> String
meioAux str n = take n str


{-
2) (2.5) Escreva uma função localizar, que indica a posição  de início de uma String dentro de outra. Caso não seja encontrada ou a String de busca seja vazia, deve ser retornado o valor 0 (zero). A contagem das posições começa no valor 1 para o primeiro caracter. Deve ser retornada a posicao da primeira ocorrencia, se houver mais de uma.
Exemplos: localizar "abc" "xyz12abrt" ----> 0
          localizar "abc" "aaabrsabcfr" --> 7
          localizar "aab" "aacrabceaabc" -> 9
          localizar "" "aacrabceaabc" ----> 0
localizar :: String -> String -> Int
-}

localizar :: String -> String -> Int
localizar _ [] = 0
localizar find str = localizarAux find str 1

localizarAux :: String -> String -> Int -> Int
localizarAux _ [] _ = 0
localizarAux find str n | (localizarAux2 find str == True) = n
                        | otherwise = localizarAux find (tail str) (n+1)

localizarAux2 :: String -> String -> Bool
localizarAux2 _ [] = False
localizarAux2 [] _ = False
localizarAux2 [x] (y:ys) = x == y
localizarAux2 (x:xs) (y:ys) | x == y = localizarAux2 xs ys
                            | otherwise = False
                            
{-
3) Uma fita infinita de papel pode ser representada por uma lista de caracteres. Esta fita possui uma cabeça de leitura/escrita que le ou escreve na posição atual da fita. Esta fita pode ser lida ou escrita (alterada) pelos seguintes comandos:
ParaFrente -- Que recebe um inteiro N como parâmetro e move a posição de leitura N posições para frente;
ParaTras -- que recebe um inteiro N como parâmetro e move a posição de leitura N posições para tras;
Escreva -- que recebe um caracter como parametro e escreve o caracter naquela posicao da fita, substituindo o caracter que estiver naquela posicao;
A fita comeca na posicao de numero 1.
Nao é preciso tratar o caso da fita se mover para uma posicao invalida (menor que 1).

data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

3.1) (2.0) Esreva uma funcao que diga a posicao final da fita, apos receber uma lista de comandos.
Exemplos:
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 6
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 7
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 7
posicaofinal "abcdefghijklmno" [] -------------------------------------------------------------------> 1
posicaofinal :: String -> [Comando] -> Int
-}                            

data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

posicaofinal :: String -> [Comando] -> Int
posicaofinal str xs = posicaofinalaux str xs 1

posicaofinalaux :: String -> [Comando] -> Int -> Int
posicaofinalaux _ [] n = n
posicaofinalaux str ((ParaFrente x):xs) n = posicaofinalaux str xs (n+x)
posicaofinalaux str ((ParaTras x):xs) n = posicaofinalaux str xs (n-x)
posicaofinalaux str (x:xs) n = posicaofinalaux str xs n

{-
3.2) (2.0) Escreva uma funcao que, dada uma lista de comandos, retorne o caracter da posicao final da cabeca de leitura, apos comandos.
Exemplos:
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 'x'
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 'y'
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 'g'
interprete "abcdefghijklmno" [] -------------------------------------------------------------------> 'a'
interprete :: String -> [Comando] -> Char
-}

interprete :: String -> [Comando] -> Char
interprete str xs = interpreteaux str xs 1

interpreteaux :: String -> [Comando] -> Int -> Char
interpreteaux str [] n = head(drop (n-1) str)
interpreteaux str ((ParaFrente x):xs) n = interpreteaux str xs (n+x)
interpreteaux str ((ParaTras x):xs) n = interpreteaux str xs (n-x)
interpreteaux str ((Escreva c):xs) n = interpreteaux ((take (n-1) str) ++ [c] ++ (drop n str)) xs n

{-
3.3) (1.0) Escreva a mesma funcao anterior, mas que mostre toda a String apos a interpretacao
estadofinal "abcdefghijklmno" [] -------------------------------------------------------------------> "abcdefghijklmno"
estadofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> "abcdexghijklmno"
estadofinal :: String -> [Comando] -> String
-}

estadofinal :: String -> [Comando] -> String
estadofinal str xs = estadofinalaux str xs 1

estadofinalaux :: String -> [Comando] -> Int -> String
estadofinalaux str [] n = str
estadofinalaux str ((ParaFrente x):xs) n = estadofinalaux str xs (n+x)
estadofinalaux str ((ParaTras x):xs) n = estadofinalaux str xs (n-x)
estadofinalaux str ((Escreva c):xs) n = estadofinalaux ((take (n-1) str) ++ [c] ++ (drop n str)) xs n

