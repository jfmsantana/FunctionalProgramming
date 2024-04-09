import Data.Char

duplica_valor :: Int -> Int
duplica_valor n = n * 2

incrementa_valor :: Int -> Int
incrementa_valor n = n + 1

quadrado_valor :: Int -> Int
quadrado_valor n = n * n


mapInt function [] = []
mapInt function (a:x) = (function a) : mapInt function x

mapInt_v2 :: (Int -> Int) -> [Int] -> [Int]
mapInt_v2 function [] = []
mapInt_v2 function (a:x) = (function a) : mapInt_v2 function x

conta_itens_lista :: [t] -> Int
conta_itens_lista [] = 0;
conta_itens_lista (_:restante) = 1 + conta_itens_lista restante


inverte_polimorfico :: [t] -> [t]
inverte_polimorfico [] = []
inverte_polimorfico (a:x) = (inverte_polimorfico x) ++ [a]

agrupa_listas_polimorfico :: [t] -> [h] -> [(t,h)]
agrupa_listas_polimorfico _ [] = []
agrupa_listas_polimorfico [] _ = []
agrupa_listas_polimorfico (a:x) (b:y) = (a,b) : agrupa_listas_polimorfico x y
-- Em List Comprehension:
-- agrupa_listas_polimorfico (a:x) (b:y) = [(a,b) | a <- [a], b <- [b]] ++ agrupa_listas_polimorfico x y 

filterDigits :: String -> String
filterDigits = filter isDigit

filterLetters :: String -> String
filterLetters = filter isLetter