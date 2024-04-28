module Conversor.FuncionesOctales
( transformOctBin
, getPow8)
where
--Funcion para buscar un digito octal y cambiarlo por uno binario
transformOctBin :: [Int] -> String
transformOctBin [] = []
transformOctBin (x:resto) 
    | x == 1 = "001" ++ transformOctBin(resto)
    | x == 2 = "010" ++ transformOctBin(resto)
    | x == 3 = "011" ++ transformOctBin(resto)
    | x == 4 = "100" ++ transformOctBin(resto)
    | x == 5 = "101" ++ transformOctBin(resto)
    | x == 6 = "110" ++ transformOctBin(resto)
    | x == 7 = "111" ++ transformOctBin(resto)
    | otherwise = "000" ++ transformOctBin(resto)

--Funcion para obtener una lista de potencias de 8
getPow8 num = reverse (1 : [8^x|x<-[1,2..num-1]])