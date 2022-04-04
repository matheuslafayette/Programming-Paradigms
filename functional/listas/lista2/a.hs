type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa xs = executaAux 0 xs

a = [("Multiplica", 2::Int), ("Soma", 5::Int), ("Subtrai", 3::Int)]

executaAux :: Int -> [(Comando, Valor)] -> Int
executaAux v [] = v
executaAux v ( ("Divide", 0) : xs ) = -666
executaAux v ( ("Multiplica", value) : xs ) = executaAux (v*value) xs
executaAux v ( ("Soma", value) : xs ) = executaAux (v+value) xs
executaAux v ( ("Subtrai", value) : xs ) = executaAux (v-value) xs
executaAux v ( ("Divide", value) : xs ) = executaAux (div v value) xs

main = do
    a <- getLine
    let result = executa (read a)
    print result