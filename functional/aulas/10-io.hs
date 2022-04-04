main :: IO()
main = do
       putStrLn "Escrevendo"
       writeFile "a.txt" "Hello\nWorld!"
       appendFile "a.txt" "\nof\nHaskell"
       putStrLn "Lendo o arquivo"
       x <- readFile "a.txt"
       putStrLn x

main1 :: IO()
main1 = do
       name <- getLine
       putStrLn "Escrevendo"
       writeFile name "Hello\nWorld!"
       appendFile name "\nof\nHaskell"
       putStrLn "Lendo o arquivo"
       x <- readFile name
       putStrLn x
