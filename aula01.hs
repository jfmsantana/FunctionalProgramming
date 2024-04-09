module Main where

main = putStrLn "Hello, World!"


soma2 :: Int -> Int -> Int
soma2 a b = a + b 
soma3 :: Int -> Int -> Int
soma3 soma2 x = soma2 + x 

raio2 :: Float -> Float
raio2 c = c * c

areaCirc :: Float -> Float
areaCirc raio2 = pi * raio2