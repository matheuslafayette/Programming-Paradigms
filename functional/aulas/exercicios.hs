type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

livros :: BancoDados -> Pessoa -> [Livro]
livros banco pessoa = [snd(dado) | dado <- banco, fst(dado) == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos banco livro = [ fst(dado) | dado <- banco, snd(dado) == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd liv = emprestimos bd liv /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd pessoa = length( livros bd pessoa )

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar xs pessoa livro = (pessoa, livro) : xs

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver banco pessoa livro = [ dado | dado <- banco, fst(dado) /= pessoa || snd(dado) /= livro ]


type Words = String

getWord :: String -> String
getWord [] = []
getWord (x:xs) | x == ' ' = []
               | otherwise = x:(getWord xs)

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs) | x /= ' ' = dropWord xs         
                | otherwise = x:xs         

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs) | x == ' ' = dropSpace xs         
                 | otherwise = x:xs            
               
splitWords :: String -> [Words]
splitWords [] = []
splitWords xs = getWord( (dropSpace xs) ) : splitWords( dropWord( (dropSpace xs) ))