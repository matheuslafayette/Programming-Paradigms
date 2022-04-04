import Text.Printf

logMes :: String -> String -> Double
logMes ys xs = (logMesSum (logMesAux (pair (dropTextData xs) (dropTextValue xs)) ys))

logMesAux :: [(Words,Double)] -> String -> [Double]
logMesAux xs str = [snd(a) | a <- xs, fst(a) == str]

logMesSum :: [Double] -> Double
--logMesSum xs = (read $ printf "%.2g" (foldl (+) 0 xs) :: Double)
logMesSum xs = (foldl (+) 0 xs)        

type Words = String

getWord :: String -> String
getWord [] = []
getWord (x:xs) | x == ';' = ""
               | otherwise = x:(getWord xs)

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs) | x /= ';' = dropWord xs         
                | otherwise = x:xs

dropWordS :: String -> String
dropWordS [] = []
dropWordS (x:xs) | x /= ' ' = dropWordS xs         
                 | otherwise = x:xs  

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs) | x == ';' = dropSpace xs         
                 | otherwise = x:xs            
               
dropSpaceS :: String -> String
dropSpaceS [] = []
dropSpaceS (x:xs) | x == ' ' = dropSpaceS xs         
                  | otherwise = x:xs   

splitWords :: String -> [Words]
splitWords [] = []
splitWords xs = getWord( (dropSpace xs) ) : splitWords( dropWord( (dropSpace xs) ))

dropTextAuxValue :: Int -> [Words] -> [Double]
dropTextAuxValue _ [] = []
dropTextAuxValue n (x:xs) | mod n 3 == 0 = (stringToDouble x):(dropTextAuxValue (n+1) xs)
                          | otherwise = dropTextAuxValue (n+1) xs

dropTextAuxData :: Int -> [Words] -> [Words]
dropTextAuxData _ [] = []
dropTextAuxData n (x:xs) | mod n 3 == 1 = dropSpaceS(dropWordS(x)):(dropTextAuxData (n+1) xs)
                         | otherwise = dropTextAuxData (n+1) xs                   

dropTextData :: String -> [Words]
dropTextData xs = dropTextAuxData 1 (splitWords xs)       


dropTextValue :: String -> [Double]
dropTextValue xs = dropTextAuxValue 1 (splitWords xs) 

minMax :: [Words] -> (Double, Double)
minMax [] = (0,0)
minMax [x] = (stringToDouble x, stringToDouble x)
minMax (x:xs) = (minn (stringToDouble x) (fst(minMax xs)), maxx (stringToDouble x) (snd(minMax xs)))

maxx :: Double -> Double -> Double
maxx a b | a > b = a
        | otherwise = b

minn :: Double -> Double -> Double
minn a b | a < b = a
         | otherwise = b

stringToDouble :: String -> Double
stringToDouble xs = read xs

pair :: [Words] -> [Double] -> [(Words,Double)]
pair _ [] = []
pair [] _ = []
pair (x:xs) (y:ys) = (x,y):( pair xs ys )