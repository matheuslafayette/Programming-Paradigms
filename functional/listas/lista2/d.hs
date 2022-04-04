data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

a = [Insert "O "]
b = [Delete 6, Insert "O "]
c = [Cursor 6, Backspace 6]
d = [Cursor 2, Insert "xxx", Insert "WWW"]

editText :: String -> [Cmd] -> String
editText str [] = str
editText str xs = editTextAux str xs 0

editTextAux :: String -> [Cmd] -> Int -> String
editTextAux str [] _ = str
editTextAux str ( (Cursor n):xs ) pos = editTextAux str xs (pos + n)
editTextAux str ( (Backspace n):xs ) pos = editTextAux (backspace str n pos) xs (pos-n)
editTextAux str ( (Insert put):xs ) pos = editTextAux (insert str put pos) xs (pos)
editTextAux str ( (Delete n):xs ) pos = editTextAux (delete str n pos) xs pos

backspace :: String -> Int -> Int -> String
backspace str n pos = ( splitText str 0 (max 0 (pos - n)) 0 ) ++ ( splitText str (pos) (length str) 0 )

insert :: String -> String -> Int -> String
insert str put pos = ( splitText str 0 pos 0 ) ++ put ++ ( splitText str (pos) (length str) 0 )

delete :: String -> Int -> Int -> String
delete str n pos = ( splitText str 0 pos 0 ) ++ ( splitText str (pos + n) (length str) 0 )

-- editTextBack :: String -> Int -> Int -> Int -> String
-- editTextBack [] _ _ _ = []
-- editTextBack (x:xs) n pos cont | ( cont < (pos-n-1) ) || ( cont >= pos ) = x : editTextBack xs n pos (cont+1)
--                                | otherwise = editTextBack xs n pos (cont+1)

splitText :: String -> Int -> Int -> Int -> String
splitText [] _ _ _ = []
splitText (x:xs) init end n | (n >= init && n < end) = x:(splitText xs init end (n+1))
                            | otherwise = (splitText xs init end (n+1))


