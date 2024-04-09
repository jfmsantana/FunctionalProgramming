--Q1
ehPrimo :: Int -> Bool
ehPrimo x 
    | (x <= 1) || (temDivisores x 2) = False
    | otherwise = True 
        where 
            temDivisores :: Int -> Int -> Bool
            temDivisores x divisor
                | divisor * divisor > x = False
                | x `mod` divisor == 0 = True
                | otherwise = temDivisores x (divisor + 1)
    
--Q2
ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int,Int,Int,Int)
ordenaEmTupla 0 0 0 0 = (0,0,0,0)
ordenaEmTupla a b c d
    | a > b = ordenaEmTupla b a c d
    | b > c = ordenaEmTupla a c b d
    | c > d = ordenaEmTupla a b d c
    | otherwise = (a, b, c, d)

--Q3
--Para terem 366 dias (bisssextos), têm de ser divisíveis por 4 e não divisíveis
--por 100, ou divisíveis por 400
quantosDias :: Int -> Int
quantosDias x 
    | (x `mod` 4 == 0 && x `mod` 100 /= 0) || (x `mod` 400 == 0) = 366
    | otherwise = 365 

--Q4
--Todos os meses dos anos não variam seus dias ao longo dos anos, exceto fevereiro que 
--tem 29 dias em anos bissextos

diasMes :: Int -> Int -> Int
diasMes a b
    | b == 2 = if ehBissexto a then 29 else 28
    | b == 4 || b == 6 || b == 9 || b == 11 = 30
    | b == 1 || b == 3 || b == 5 || b == 7 || b == 8 || b == 10 || b == 12 = 31
    | otherwise = -1
        where
            ehBissexto :: Int -> Bool
            ehBissexto a 
                | (a `mod` 4 == 0 && a `mod` 100 /= 0) || (a `mod` 400 == 0) = True
                | otherwise = False

--Q5
dia :: Int -> Int -> Int -> Int
dia a b c 
    | b < 1 || b > 12 || c < 1 || c > 31 = -1
    | b == 1 && c <= 31 = c
    | b == 2 && c <= (if ehBissexto a then 29 else 28) = if c == 29 then 60 else 31 + c 
    | b == 3 && c <= 31 = if ehBissexto a then 60 + c else 59 + c 
    | b == 4 && c <= 30 = if ehBissexto a then 91 + c else 90 + c 
    | b == 5 && c <= 31 = if ehBissexto a then 121 + c else 120 + c
    | b == 6 && c <= 30 = if ehBissexto a then 152 + c else 151 + c
    | b == 7 && c <= 31 = if ehBissexto a then 182 + c else 181 + c
    | b == 8 && c <= 31 = if ehBissexto a then 213 + c else 212 + c
    | b == 9 && c <= 30 = if ehBissexto a then 244 + c else 243 + c
    | b == 10 && c <= 31 = if ehBissexto a then 274 + c else 273 + c
    | b == 11 && c <= 30 = if ehBissexto a then 305 + c else 304 + c
    | b == 12 && c <= 31 = if ehBissexto a then 335 + c else 334 + c
    | otherwise = -1
        where
            ehBissexto :: Int -> Bool
            ehBissexto a 
                | (a `mod` 4 == 0 && a `mod` 100 /= 0) || (a `mod` 400 == 0) = True
                | otherwise = False

--Q6
maioremenor :: [Int] -> (Int,Int)
maioremenor [] = (0,0)
maioremenor (a:x) = (menor (a:x), maior (a:x))
    where
        menor :: [Int] -> Int
        menor [a] = a
        menor (a:x) = menor_valor a (menor x)
            where 
                menor_valor :: Int -> Int -> Int
                menor_valor a b 
                    | a < b = a 
                    | otherwise = b
        maior :: [Int] -> Int
        maior [a] = a
        maior (a:x) = maior_valor a (maior x)
            where 
                maior_valor :: Int -> Int -> Int
                maior_valor a b 
                    | a > b = a 
                    | otherwise = b  

--Q7
ordena :: [Int] -> [Int]
ordena [] = []
ordena (a:x) = menor a (ordena x)
    where
        menor :: Int -> [Int] -> [Int]
        menor a [] = [a]
        menor a (b:x)
            | a < b = a : (b:x)
            | otherwise = b : menor a x 

-- ordena :: [Int] -> [Int]
-- ordena (a:x) = [b | b <- (a:x), b < a] ++ [a] ++ [b | b <- (a:x), b > a]


--Q8
repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:x) = [a | _ <- [1..a]] ++ repeteElemento x 
-- Cria-se uma lista com elemento a repetido a vezes    , e concatena-se essa lista
-- com o resultado da chamada recursiva de repeteElemento

--Q9
serie :: Int -> Int -> Int
serie _ 0 = 0
serie x n
    | n `mod` 2 /= 0 = n `div` x + serie x (n-1)
    | otherwise = x `div` n + serie x (n-1)


