length' :: [t] -> Int
length' [] = 0
length' (a:as) = 1 + length' as

(+++) :: [t] -> [t] -> [t]
[] +++ y = y
(x:xs) +++ y = x : (xs +++ y)

sumList :: [Int] -> Int
{-sumList as | as == [] = 0
           | otherwise = (head as) + sumList(tail as)-}
sumList [] = 0
sumList (a:as) = a + sumList as

double :: [Int] -> [Int]
double [] = []
double (a:as) = (2*a) : double as

member :: [Int] -> Int -> Bool
member [] n = False
member (a:as) n | a == n = True
                | otherwise = member as n

digits :: String -> String
digits [] = []
digits (a:as) | a <= '9' && a >= '0' = a : digits as
              | otherwise = digits as

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] _ = []
sumPairs _ [] = []
sumPairs (a:as) (b:bs) = (a + b) : sumPairs as bs

maiorLista :: [Int] -> Int
maiorLista [] = minBound :: Int
maiorLista (x:[]) = x
maiorLista (x:xs) | x > maiorLista xs = x
                  | otherwise = maiorLista xs

maiorMenorQue :: [Int] -> Int -> ([Int], [Int])
maiorMenorQue [] n = ([], [])
maiorMenorQue (a:as) n | a > n = (xs, a:ys)
                       | otherwise = (a:xs, ys)
    where
        (xs, ys) = maiorMenorQue as n                        

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:as) = quickSort xs ++ [a] ++ quickSort ys
    where
        (xs, ys) = maiorMenorQue as a

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

listFib :: Int -> [Int]
listFib 0 = []
listFib n = listAux n 0
    where
        listAux :: Int -> Int -> [Int]
        listAux 0 x = []
        listAux n x | isPair (fib x) = fib x : listAux (n-1) (x+1)
                    | otherwise = listAux n (x+1)

        isPair :: Int -> Bool
        isPair n = mod n 2 == 0

{-sortByDigitSum :: [Int] -> [Int]
sortByDigitSum [] = []
sortByDigitSum (a:as) = sortByDigitSum (menorQueDigit as a) ++ [a] ++ sortByDigitSum (maiorQueDigit as a)
 where
    menorQueDigit :: [Int] -> Int -> [Int]
    menorQueDigit [] n = []
    menorQueDigit (a:as) n | valueFromDigits a <= valueFromDigits n = a : menorQueDigit as n
                           | otherwise = menorQueDigit as n

    maiorQueDigit :: [Int] -> Int -> [Int]
    maiorQueDigit [] n = []
    maiorQueDigit (a:as) n | valueFromDigits a > valueFromDigits n = a : maiorQueDigit as n
                           | otherwise = maiorQueDigit as n
-}

sortByDigitSum :: [Int] -> [Int]
sortByDigitSum [] = []
sortByDigitSum (a:as) = sortByDigitSum (xs) ++ [a] ++ sortByDigitSum (ys)
 where
    (xs, ys) = maiorMenorQueDigits as a

maiorMenorQueDigits :: [Int] -> Int -> ([Int], [Int])
maiorMenorQueDigits [] n = ([], [])
maiorMenorQueDigits (a:as) n | valueFromDigits a >= valueFromDigits n = (xs, a:ys)
                             | otherwise = (a:xs, ys)
    where
        (xs, ys) = maiorMenorQueDigits as n                      

valueFromDigits :: Int -> Int
valueFromDigits n | n <= 9 = n
                  | otherwise = mod n 10 + valueFromDigits (div n 10)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort as = merge (mergeSort xs) (mergeSort ys)
    where
        (xs, ys) = halfList as

halfList :: [Int] -> ([Int], [Int])
halfList as = halfListAux (div (length as) 2) as

halfListAux :: Int -> [Int] -> ([Int], [Int])
halfListAux 0 as = ([],as)
halfListAux _ [] = ([],[])
halfListAux n (a:as) = ((a:x), y)
    where
        (x, y) = halfListAux (n-1) as

merge :: [Int] -> [Int] -> [Int]
merge [] as = as
merge as [] = as
merge (a:as) (b:bs) | a < b = a : (merge as (b:bs))
                    | otherwise = b : (merge (a:as) bs)

mergeSortDigitSum :: [Int] -> [Int]
mergeSortDigitSum [] = []
mergeSortDigitSum [x] = [x]
mergeSortDigitSum as = mergeDigitSum (mergeSortDigitSum xs) (mergeSortDigitSum ys)
    where
        (xs, ys) = halfList as

        mergeDigitSum :: [Int] -> [Int] -> [Int]
        mergeDigitSum [] as = as
        mergeDigitSum as [] = as
        mergeDigitSum (a:as) (b:bs) | valueFromDigits a <= valueFromDigits b = a : (mergeDigitSum as (b:bs))
                                    | otherwise = b : (mergeDigitSum (a:as) bs)