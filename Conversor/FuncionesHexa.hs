module Conversor.FuncionesHexa
( transformHexaBin
, transformHexa
, getPow16)
where
--Funcion que en base a un digito hexadecimal lo transforma en uno binario
transformHexaBin :: [[Char]] -> String
transformHexaBin [] = []
transformHexaBin (x:resto)
    | x == "1" = "0001" ++ transformHexaBin(resto)
    | x == "2" = "0010" ++ transformHexaBin(resto)
    | x == "3" = "0011" ++ transformHexaBin(resto)
    | x == "4" = "0100" ++ transformHexaBin(resto)
    | x == "5" = "0101" ++ transformHexaBin(resto)
    | x == "6" = "0110" ++ transformHexaBin(resto)
    | x == "7" = "0111" ++ transformHexaBin(resto)
    | x == "8" = "1000" ++ transformHexaBin(resto)
    | x == "9" = "1001" ++ transformHexaBin(resto)
    | x == "A" = "1010" ++ transformHexaBin(resto)
    | x == "B" = "1011" ++ transformHexaBin(resto)
    | x == "C" = "1100" ++ transformHexaBin(resto)
    | x == "D" = "1101" ++ transformHexaBin(resto)
    | x == "E" = "1110" ++ transformHexaBin(resto)
    | x == "F" = "1111" ++ transformHexaBin(resto)
    | otherwise = "0000" ++ transformHexaBin(resto)

--Para convertir una lista numeros hexadecimales en una de numeros enteros
transformHexa :: [[Char]] -> [[Char]]
transformHexa [] = []
transformHexa (x:rest)
    | x == "A" = "10" : transformHexa(rest)
    | x == "B" = "11": transformHexa(rest)
    | x == "C" = "12": transformHexa(rest)
    | x == "D" = "13": transformHexa(rest)
    | x == "E" = "14": transformHexa(rest)
    | x == "F" = "15": transformHexa(rest)
    | otherwise = x: transformHexa(rest)

--Funcion para obtener una lista de potencias de 16
getPow16 num = reverse (1 : [16^x|x<-[1,2..num-1]])