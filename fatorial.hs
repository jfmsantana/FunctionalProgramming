fatorial :: Integer -> Integer
fatorial num
    | num == 0 = 1                           
    | otherwise = num * (fatorial (num - 1))