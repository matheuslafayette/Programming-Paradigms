
data Evento = Normal | Denied
    deriving (Show)

type Log = (Data, Hora, Evento, Ident)


  
logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;" :: String

log2 = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;" :: String



--1)

normalDenied :: String -> (Int, Int)
normalDenied str = normalDeniedAux (takeOutPoint str) (0, 0) 0

normalDeniedAux :: [String] -> (Int, Int) -> Int -> (Int, Int)
normalDeniedAux [] (vn, vd) _ = (vn, vd)
normalDeniedAux (x:xs) (vn, vd) n | mod n 4 == 2 && x == "Normal" = normalDeniedAux xs (vn + 1, vd) (n+1)
                                  | mod n 4 == 2 && x == "Denied" = normalDeniedAux xs (vn, vd + 1) (n+1)
                                  | otherwise = normalDeniedAux xs (vn, vd) (n+1)

takeOutPoint :: String -> [String]
takeOutPoint [] = []
takeOutPoint str = (takeWhile difPoint str) : takeOutPoint (dropWhile ndifPoint (dropWhile difPoint str) )

difPoint :: Char -> Bool
difPoint c = c /= ';' && c /= '\n'

ndifPoint :: Char -> Bool
ndifPoint c = c == ';' || c == '\n'

dift :: Char -> Bool
dift c = c /= '-'

--2)

dias :: String -> [Int]
dias str = diasAux (takeOutPoint str) 0

diasAux :: [String] -> Int -> [Int]
diasAux [] _ = []
diasAux (x:xs) n | mod n 4 == 0 =  (justDay x): (diasAux xs (n+1))
                 | otherwise = (diasAux xs (n+1))

justDay :: String -> Int
justDay str = read (tail (dropWhile dift (tail (dropWhile dift str))))::Int

acessosPorDia :: String -> [(Int, Int)]
acessosPorDia str = acessosPorDiaAux (dias str)

acessosPorDiaAux :: [Int] -> [(Int, Int)]
acessosPorDiaAux [] = []
acessosPorDiaAux (x:xs) = (x, contTimes (x:xs) x) : (acessosPorDiaAux [a | a <- xs, a /= x])

contTimes :: [Int] -> Int -> Int
contTimes [] _ = 0
contTimes (x:xs) n | x == n = 1 + contTimes xs n
                   | otherwise = contTimes xs n


--3)

acessosPorUsuario :: String -> [(Int, Int)]   
acessosPorUsuario str = acessosPorUsuarioAux (users str)

users :: String -> [Int]
users str = usersAux (takeOutPoint str) 0

usersAux :: [String] -> Int -> [Int]
usersAux [] _ = []
usersAux (x:xs) n | mod n 4 == 3 =  ((read x)::Int) : (usersAux xs (n+1))
                  | otherwise = (usersAux xs (n+1))

acessosPorUsuarioAux :: [Int] -> [(Int, Int)]
acessosPorUsuarioAux [] = []
acessosPorUsuarioAux (x:xs) = (x, contTimes (x:xs) x) : (acessosPorUsuarioAux [a | a <- xs, a /= x])

--4)
type Dia = String
type Hora = String
type Usuario = String

data LogEntry = Permitido Dia Hora Usuario
              | Negado Dia Hora Usuario
        deriving (Show)



