{- Int para valores inteiros limitados
 Integer para inteiros, mas sem limitação
 Float
 Double
 Char 
 String
 Rational para fracionário, sem erro de arredondamento
 Bool para true or false 

 Tuplas são denotadas por parenteses e separados por vírgulas-}
 
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)
fatorial' n = (fatorial n) * (fatorial 1)