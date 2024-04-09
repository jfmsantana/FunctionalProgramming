--Q1
retornaNElementos :: Int -> [Int] -> [Int]
retornaNElementos _ [] = []
retornaNElementos 0 _ = []
retornaNElementos n (a:x) = [a] ++ retornaNElementos (n-1) x

--Q2
existe :: Int -> [Int] -> Bool
existe n [] = False
existe n (a:x)
    | (a == n) = True
    | otherwise = existe n x

--Q3
maiorLista :: [Int] -> Int
maiorLista [] = 0
maiorLista (a:x) 
    | (a > maiorLista x) = a
    | otherwise = maiorLista x

--Q4
inverte :: [Int] -> [Int]
inverte [] = []
inverte (a:x) = inverte x ++ [a]

--Q5
ultimo :: [Int] -> Int
ultimo [] = 0
ultimo [a] = a
ultimo (a:x) = ultimo x

--Q6
kesimo :: Int -> [Int] -> Int
kesimo _ [] = 0
kesimo k (a:x)
    | (k == 1) = a 
    | otherwise = kesimo (k-1) x 