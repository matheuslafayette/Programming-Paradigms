multiply :: Int -> Int -> Int
multiply a b = a * b

doubleList :: [Int] -> [Int]
doubleList = map (*2)
-- doubleList = map (multiply 2)
--doubleList xs = map (multiply 2) xs
--doubleList xs = map ((*) 2) xs
--doubleList xs = map (*2) xs


whiteSpace :: [Char] 
whiteSpace = " "

noWhiteSpaces :: [Char] -> [Char]
noWhiteSpaces = filter (\ch ->  not(elem ch whiteSpace))

