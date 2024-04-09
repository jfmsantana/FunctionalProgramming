--Q1
ehPerfeito :: Int -> Bool
ehPerfeito x
    | x == somaDivisores x (x-1) = True
    | otherwise = False
          where
            somaDivisores :: Int -> Int -> Int
            somaDivisores a b 
                | b == 0 = 0
                | a == 1 = 1
                | a `mod` b == 0 = b + somaDivisores a (b-1)
                | otherwise = somaDivisores a (b-1)

--Q2
contaString :: String -> [(Char, Int)]
contaString "" = []
contaString (x:xs) = (x, contaOcorrencias x (x:xs)) : contaString (removeChar x xs)
    where
        contaOcorrencias :: Char -> String -> Int
        contaOcorrencias _ "" = 0
        contaOcorrencias c (y:ys)
            | c == y = 1 + contaOcorrencias c ys
            | otherwise = contaOcorrencias c ys

        removeChar :: Char -> String -> String
        removeChar _ "" = ""
        removeChar c (y:ys)
            | c == y = removeChar c ys
            | otherwise = y : removeChar c ys

--Q3
inverteString :: String -> String
inverteString "" = ""
inverteString (x:xs) = inverteString xs ++ [x]

--Q4
squares :: Int -> Int
squares x = x * x 

--Q5
produtoCartesiano :: [Int] -> [Int] -> [(Int, Int)]
produtoCartesiano [] _ = []  
produtoCartesiano _ [] = []  
produtoCartesiano (x:xs) (b:y) = [(x, b) | b <- (b:y)] ++ produtoCartesiano xs (b:y)

produtoCartesianoInverso :: [Int] -> [Int] -> [(Int, Int)]
produtoCartesianoInverso _ [] = []  
produtoCartesianoInverso [] _ = []
produtoCartesianoInverso (x:xs) (b:y) = [(b, x) | x <- (x:xs)] ++ produtoCartesianoInverso (x:xs) y  

produtoCartesianoOrdenado :: [Int] -> [Int] -> [(Int, Int)]
produtoCartesianoOrdenado (x:xs) (b:y) = produtoCartesiano (x:xs) (b:y) ++ produtoCartesianoInverso (x:xs) (b:y)


ehPositivo :: Int -> Bool
ehPositivo x 
    | x > 0 = True
    | otherwise = False
--Chamar no terminal: filter ehPositivo [1,-2,3] 

--Q7
dobro :: [Int] -> [Int]
dobro [] = []
--Aplicando o dobro
dobro (a:x) = [(*2)a | a <- (a:x)]
-- Poderia ser: dobro (a:x) = map (*2) (a:x)
--Ao chamar no terminal: foldr1 (+) (dobro [1,2,3]), por exemplo
--Ou ainda apenas chamar no terminal: foldr1 (+) (map (*2) [1,2,3])


--Q8
--Apenas precisa chamar no terminal:
-- foldr1 (++) [LISTA DE STRINGS] 
-- Ou então chamar a função: 

--concatena :: [String] -> String
--concatena (a:x) = foldr (++) (a:x)

removeAspas :: [String] -> [String]
removeAspas (a:x) = map (filter (/= '"')) (a:x)