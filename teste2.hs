fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x - 1)

inverte :: Int -> Int
inverte x
    | x < 10 = x
    | otherwise = (x `mod` 10) 

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y
    | y == 0 = 0
    | x == 0 = 0
    | otherwise = soma x (mult x (y - 1))
                

        