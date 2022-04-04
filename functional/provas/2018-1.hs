{-
1) Na copa do mundo de futebol, os grupos contém 4 times, e avançam para a fase seguinte os que tem maior número de pontos, sendo 3 pontos por uma vitória, 1 por um empate, e zero por uma derrota.
Vamos representar os times e jogos com os tipos de dados e sinonimos de tipos abaixo:

data Time = Egito | Russia | Arabia | Uruguai 
          | Ira | Marrocos | Portugal | Espanha de

type Jogo = (Time, Int, Int, Time) 
Por exemplo: Egito 3 x 1 Russia será representado por (Egito, 3, 1, Russia)

a) (2.0) Defina uma função que, dado um time e uma lista de jogos, informe quantos gols aquele time fez.
gols :: Time -> [Jogo] -> Int

b) (2.0) Defina uma função que, dado um time e uma lista de jogos, qual o seu saldo de gols naquele conjunto de jogos (gols feitos - gols tomados).
saldo :: Time -> [Jogo] -> Int

c) (2.0) Defina uma função que, dado um time e uma lista de jogos, informe quantos pontos ele obteve naquele conjunto de jogos.
pontos :: Time -> [Jogo] -> Int

d) (1.0) Defina um tipo de dados para caracterizar um Grupo, que contém o nome do grupo (os grupos vão da letra 'A' à Letra 'H') e 4 times.

e) (3.0)  Feito isso, defina uma função que, dado um Grupo e uma lista de jogos, retorne o par de times que estão classificados.
   Os classificados são: os dois com maior número de pontos; em caso de empate, usa-se o saldo de gols; em caso de continuar empate usa-se o número de gols feitos (há regras adicionais, mas vamos implementar apenas essas 3).
exemplos de grupos são: Grupo A: Egito, Russia, Arabia e Uruguai; 
                        Grupo B: Ira, Marrocos, Portugal e Espanha;
classificados :: Grupo -> [Jogo] -> (Time, Time)
      
você deve definir e testar vários conjuntos de jogos para validar sua implementação.
jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]
-}

data Time = Egito | Russia | Arabia | Uruguai 
          | Ira | Marrocos | Portugal | Espanha
          deriving (Show, Eq)

type Jogo = (Time, Int, Int, Time)

jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]

t1 :: Jogo -> Time
t1 (time1, gols1, gols2, time2) = time1

t2 :: Jogo -> Int
t2 (time1, gols1, gols2, time2) = gols1

t3 :: Jogo -> Int
t3 (time1, gols1, gols2, time2) = gols2

t4 :: Jogo -> Time
t4 (time1, gols1, gols2, time2) = time2

--a)
gols :: Time -> [Jogo] -> Int
gols team [] = 0
gols team (x:xs) | team == (t1 x) = t2 x + gols team xs
                 | team == (t4 x) = t3 x + gols team xs
                 | otherwise = gols team xs

--b)
saldo :: Time -> [Jogo] -> Int
saldo team [] = 0
saldo team (x:xs) | team == (t1 x) = t2 x - t3 x + saldo team xs
                  | team == (t4 x) = t3 x - t2 x + saldo team xs
                  | otherwise = saldo team xs

--c)
pontos :: Time -> [Jogo] -> Int
pontos team [] = 0
pontos team (x:xs) | team == (t1 x) || team == (t4 x) = pontosAux team x + pontos team xs
                   | otherwise = pontos team xs

pontosAux :: Time -> Jogo -> Int
pontosAux team game | team == (t1 game) && (t2 game) > (t3 game) = 3
                    | team == (t4 game) && (t3 game) > (t2 game) = 3
                    | (t2 game) == (t3 game) = 1
                    | otherwise = 0

--d)
type Grupo = (Char, Time, Time, Time, Time)
grupoteste = ('A', Portugal, Egito, Arabia, Ira) :: Grupo

--e)
classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados grupo jogos = (firstoneaux (pontosGols grupo jogos), firstoneaux (removelist (pontosGols grupo jogos) (firstoneaux (pontosGols grupo jogos)) ))
--Time, Pontos, Saldo
pontosGols :: Grupo -> [Jogo] -> [(Time, Int, Int)]
pontosGols (name, team1, team2, team3, team4) games = (team1, pontos team1 games, saldo team1 games) : (team2, pontos team2 games, saldo team2 games) : (team3, pontos team3 games, saldo team3 games) : (team4, pontos team4 games, saldo team4 games) : []

firstone :: [(Time, Int, Int)] -> (Time, Int, Int) -> Time
firstone [] (team1, points1, gols1) = team1
firstone ((team, points, gols):xs) (team1, points1, gols1) | points > points1 || (points == points && gols > gols1) = firstone xs (team, points, gols) 
                                                           | otherwise = firstone xs (team1, points1, gols1)

firstoneaux :: [(Time, Int, Int)] -> Time
firstoneaux (x:xs) = firstone xs x

removelist :: [(Time, Int, Int)] -> Time -> [(Time, Int, Int)]
removelist pointsgols teamremove = [(team, points, gols) | (team, points, gols) <- pointsgols, team /= teamremove]