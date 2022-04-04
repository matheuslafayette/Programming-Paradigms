doubleList :: [Int] -> [Int]
doubleList xs = [2*a | a <- xs]

isEven :: Int -> Bool
isEven a = mod a 2 == 0

doubleIfEven :: [Int] -> [Int]
doubleIfEven xs = [2*a | a <- xs, isEven a]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs xs = [a+b | (a,b) <- xs]

digits :: String -> String
digits xs = [ch | ch <- xs, ch < '9' && ch >= '0']

isDigit :: Char -> Bool
isDigit ch = ch < '9' && ch >= '0'

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = less ++ [x] ++ bigger
    where
        less = quicksort [y | y <- xs, y < x]
        bigger = quicksort [y | y <- xs, y >= x]