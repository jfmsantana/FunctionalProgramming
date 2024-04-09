--Q1
ehPar :: Int -> Bool
ehPar x
    | x `mod` 2 == 0 = True
    | otherwise = False

maxi2 :: Int -> Int -> Int
maxi2 x y
    | x > y = x
    | otherwise = y

naoVazia :: String -> Bool
naoVazia s 
    | s == "" = False
    | otherwise = True
