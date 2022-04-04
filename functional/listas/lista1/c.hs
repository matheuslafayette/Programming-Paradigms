minMaxCartao :: String -> (Double, Double)
minMaxCartao xs = minMax (dropText xs)

type Words = String

getWord :: String -> String
getWord [] = []
getWord (x:xs) | x == ';' = ""
               | otherwise = x:(getWord xs)

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs) | x /= ';' = dropWord xs         
                | otherwise = x:xs         

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs) | x == ';' = dropSpace xs         
                 | otherwise = x:xs            
               
splitWords :: String -> [Words]
splitWords [] = []
splitWords xs = getWord( (dropSpace xs) ) : splitWords( dropWord( (dropSpace xs) ))

dropTextAux :: Int -> [Words] -> [Words]
dropTextAux _ [] = []
dropTextAux n (x:xs) | mod n 3 == 0 = x:(dropTextAux (n+1) xs)
                     | otherwise = dropTextAux (n+1) xs

dropText :: String -> [Words]
dropText xs = dropTextAux 1 (splitWords xs)                  

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