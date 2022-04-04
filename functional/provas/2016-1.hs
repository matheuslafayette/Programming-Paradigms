import Data.Char
type Chave = [(Char, Char)]

rot13parcial :: Chave
rot13parcial = [('a', 'n'), ('b', 'o'), ('c', 'p'), ('d', 'p'), ('e', 'r'), ('f', 's'), ('g', 't'), ('h', 'u'), ('i', 'v'), ('j', 'w'), ('k', 'x'), ('l', 'y'), ('m', 'z')]

cipher :: Chave -> String -> String
cipher key [] = ""
cipher key (x:xs) = (findkey key x) : (cipher key xs)

findkey :: Chave -> Char -> Char
findkey [] c = c
findkey (k:ks) c | c == (fst k) = snd k
                 | otherwise = findkey ks c

inverteChave :: Chave -> Chave
inverteChave keys = map f keys

f :: (Char, Char) -> (Char, Char)
f (a, b) = (b, a)

type FuncaoChave = (Char -> Char)
func :: FuncaoChave
func 'z' = 'a'
func c = chr(ord c + 1)

cipherf :: FuncaoChave -> String -> String
cipherf fu str = map fu str

