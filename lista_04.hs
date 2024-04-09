import Data.Char 

lista_reversa :: [t] -> [t]
lista_reversa [] = []
lista_reversa (a:x) = lista_reversa x ++ [a]

duplas_tuplas :: [t] -> [u] -> [(t,u)]
duplas_tuplas x y = [(a,b)|a <- x, b <- y]

digits :: Char -> Bool
digits c = c `elem` ['0'..'9']

par :: Int -> Bool
par n = (n `mod` 2 == 0)

letters :: String -> String
letters text = filter isLetter text
  where
    isLetter :: Char -> Bool
    isLetter c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

--------------------------------------------------------------------------

-- Função para retornar o maior valor de uma lista
maior :: [Int] -> Int
maior [] = 0
maior [x] = x 
maior (a:x) = maior_valor a (maior x)
  where
    maior_valor :: Int -> Int -> Int
    maior_valor a b
      | a > b = a
      | otherwise = b    

lista_reversa :: [t] -> [t]
lista_reversa [] = []
lista_reversa (a:x) = lista_reversa x ++ [a]

duplas_tuplas :: [t] -> [u] -> [(t,u)]
duplas_tuplas x y = [(a,b) | a <- x, b <- y]

digits :: [Char] -> Int
digits xs = length (filter isDigit x)

