--funcao de alta ordem
--map -> mapeia elementos - em cada elemento da lista é aplicada uma funçao
--fold -> reduce - lista é reduzida a um elemento
--filter -> filtra os elementos q atendem determinada funcao

--MAP
applyBinOper :: (t -> t -> t) -> t -> t -> t
applyBinOper f x y = f x y

times2 :: Int -> Int
times2 x = x * 2

sqr :: Int -> Int
sqr x = x * x

-- mapp :: (t -> u) -> [t] -> [u]
-- mapp f [] = []
-- mapp f (x:xs) = (f x):(mapp f xs)

mapp :: (t -> u) -> [t] -> [u]
mapp f xs = [f a | a <- xs]

doubleList :: [Int] -> [Int]
doubleList xs = mapp times2 xs

sqrList :: [Int] -> [Int]
sqrList xs = mapp sqr xs

--FOLD
maxFun :: (Int -> Int) -> [Int] -> Int
maxFun f [x] = f x
maxFun f (x:xs) = max (f x) (maxFun f xs)

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = fold (*) [1..n]

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n = ( (f n) > (f (n-1)) ) && isCrescent f (n-1)

sumSqr :: [Int] -> Int
sumSqr xs = foldr (+) 0 (mapp sqr xs)

--FILTER
filter' :: (t -> Bool) -> [t] -> [t]
filter' f xs = [a | a <- xs, f a]

takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (t -> Bool) -> [t] -> [t]                    
dropWhile' f [] = []
dropWhile' f xxs@(x:xs) | f x = dropWhile' f xs
                        | otherwise = xxs        
                    
greaterZero :: [Int] -> [Int]
greaterZero xs = filter' (> 0) xs

--EXEMPLOS           
maiores :: [[Int]] -> [Int]
maiores xs = map (fold comp) xs
    where
        comp a b | a > b = a
                 | otherwise = b

--funçoes como valores
iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f).f