soma_lista :: [Int] -> Int
soma_lista [] = 0
soma_lista (a:x) = a + soma_lista x

multiplica_lista :: [Int] -> Int -> [Int]
multiplica_lista list x = [x * a | a <- list]

incrementa_lista :: [Int] -> Int -> [Int]
incrementa_lista list x = [x + a | a <- list]

filtra_valores_pares :: [Int] -> [Int]
filtra_valores_pares list = [a | a <- list, even a]

filtra_valores_impares :: [Int] -> [Int]
filtra_valores_impares list = [a | a <- list, odd a]

eh_multiplo_de :: Int -> Int -> Bool
eh_multiplo_de a x = ( a `mod` x == 0 )

filtra_multiplos_de_valor :: [Int] -> Int -> [Int]
filtra_multiplos_de_valor list x = [a | a <- list, eh_multiplo_de a x]

constroi_lista = [x*x | x <- [9 .. 39], even x]

-- Exercícios

busca_quantidade_valores :: [Int] -> Int -> [Int]
busca_quantidade_valores [] _ = []
busca_quantidade_valores _ 0 = []
busca_quantidade_valores (a:x) n = a : (busca_quantidade_valores x (n - 1))

inverte_lista :: [Int] -> [Int]
inverte_lista [] = []
inverte_lista (a:x) = (inverte_lista x) ++ [a]

agrupa_listas :: [Int] -> [Char] -> [(Int,Char)]
agrupa_listas _ [] = []
agrupa_listas [] _ = []
agrupa_listas (a:x) (b:y) = [(a,b) | a <- [a], b <- [b]] ++ agrupa_listas x y 


acha_maior :: [Int] -> Int
acha_maior [] = 0
acha_maior (a:x) = maior_valor a (acha_maior x)
    where
        maior_valor :: Int -> Int -> Int
        maior_valor a b
            | a > b = a
            | otherwise = b


adiciona_sem_repeticao2 :: Eq t => [t] -> t -> [t]
adiciona_sem_repeticao2 list x
    | (contem list x) == True = list
    | otherwise = list ++ [x] 
        where 
            contem :: Eq t => [t] -> t -> Bool
            contem [] _ = False
            contem (a:b) x = (x == a) || (contem b x)

adiciona_sem_repeticao :: [Int] -> Int -> [Int]
adiciona_sem_repeticao list x
    | (contem list x) == True = list
    | otherwise = list ++ [x] 
        where 
            contem :: [Int] -> Int -> Bool
            contem [] _ = False
            contem (a:b) x = (x == a) || (contem b x)





-- Exercícios
--Testes
retornaNElementos :: [Int] -> Int -> [Int]
retornaNElementos _ 0 = []
retornaNElementos [] _ = []
retornaNElementos (a:x) n = a : (retornaNElementos x (n - 1))

