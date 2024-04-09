dist_euclidiana :: Float -> Float -> Float -> Float -> Float
dist_euclidiana x1 x2 y1 y2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)

raizEq2 :: Float -> Float -> Float -> String
raizEq2 a b c 
  | delta > 0  = "Duas raízes reais determinadas" 
  | delta == 0 = "Uma raiz real" 
  | otherwise  = "Nenhuma raiz real"
    where
        delta = (b^2) - (4.0 * a * c)