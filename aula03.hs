type Meu_tipo = (String, Float, Char)

pessoa :: Float -> Meu_tipo
pessoa rg
	| rg == 1 = ("Joao Silva",12,'m')
	| rg == 2 = ("Jonas Souza",81,'m')
	| rg == 3 = ("Joice Silva",12,'f')
	| rg == 4 = ("Janete Souza",10,'f')
	| rg == 5 = ("Jocileide Strauss",21,'f')
	| otherwise = ("Nao há mais ninguem",0,'x')

contaFeminino :: Float -> Int
contaFeminino rg 
    | rg == 1 = ehFeminino (pessoa 1)
    | otherwise = ehFeminino (pessoa rg) + contaFeminino (rg - 1)



ehFeminino :: Meu_tipo -> Int
ehFeminino (_, _, s)
    | s == 'f' = 1
    | otherwise = 0 



type Professor = (Int, String, String, Char)

base :: Int -> Professor
base x
	|x == 0 = (1793, "Pedro Paulo", "MESTRE",'M')
	|x == 1 = (1797, "Joana S. Alencar", "MESTRE",'M')
	|x == 2 = (1534, "Joao de Medeiros", "DOUTOR",'F')
	|x == 3 = (1267, "Claudio Cesar de Sá", "DOUTOR",'M')
	|x == 4 = (1737, "Paula de Medeiros", "MESTRE",'F')
	|x == 5 = (1888, "Rita de Matos", "MESTRE",'F')
	|x == 6 = (1356, "Rodolfo Roberto", "DOUTOR", 'M')
	|x == 7 = (1586, "Célia Maria de Sousa", "DOUTOR", 'F')
	|x == 8 = (1800, "Josimar Justino", "MESTRE", 'M')
	|x == 9 = (1698, "Tereza C. Andrade", "MESTRE",'F')
	|x == 10 = ( 0, "" , "" ,'0')

ehDoutor :: Professor -> Int
ehDoutor (_, _, t, _)
    | t == "DOUTOR" = 1
    | otherwise = 0

contaDoutor :: Int -> Int
contaDoutor x
    | x == 0 = ehDoutor (base 0)
    | otherwise = ehDoutor (base x) + contaDoutor (x - 1)

ehMulher :: Professor ->  Int
ehMulher (_, _, _, s)
    | s == 'F' = 1
    |otherwise = 0

contaMulher :: Int -> Int
contaMulher x
    | x == 0 = ehMulher (base 0)
    | otherwise = ehMulher (base x) + contaMulher (x - 1)

ehMestre_e_Masc :: Professor -> Int
ehMestre_e_Masc (_, _, t, s)
    | (s == 'M' && t == "MESTRE") = 1
    | otherwise = 0

contaMestre_e_Masc :: Int -> Int
contaMestre_e_Masc x 
    | x == 0 = ehMestre_e_Masc (base 0)
    | otherwise = ehMestre_e_Masc (base x) + contaMestre_e_Masc (x - 1)


menor :: Professor -> Professor -> Professor
menor z y
    |x1 <= x2 = z
    |otherwise = y
        where
            x1 = matricula z
            x2 = matricula y

matricula :: Professor -> Int
matricula (m, _, _, _) = m


menor_matricula :: Professor -> Int
menor_matricula x
    | x == 0 = (base 0)
    | otherwise = menor (base 0)(menor_matricula(x - 1))