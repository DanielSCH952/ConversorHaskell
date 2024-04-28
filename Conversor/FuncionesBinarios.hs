module Conversor.FuncionesBinarios
( completeForHexa
, completeForOctal
, getlista3
, getlista4
, searchOctal 
, searchHexa
, getPow2)
where

--Funcion para completar a digitos de 4
completeForHexa:: String -> String
completeForHexa txt = complete
    where
        complete = replicate(searchMultiple4(length txt) - length txt) '0' ++ txt

        
--Funcion para completar a digitos de 3
completeForOctal :: String -> String
completeForOctal num = numComplete
    where
        numComplete = replicate (searchMultiple3 (length num) - length num) '0' ++ num
--Funcion para partir cada 3 elementos una lista
getlista3 :: Eq a => [a] ->[[a]]
getlista3 [] = []
getlista3 xs = take 3 xs : getlista3 (drop 3 xs)

--Funcion para partir cada 4 elementos una lista
getlista4 :: Eq a => [a] -> [[a]]
getlista4 [] = []
getlista4 lst = take 4 lst : getlista4 (drop 4 lst)

--Funcion para comparar y retornar el numero en octal
searchOctal :: String -> String
searchOctal num 
    | num == "001" = "1"
    | num == "010" = "2"
    | num == "011" = "3"
    | num == "100" = "4"
    | num == "101" = "5"
    | num == "110" = "6"
    | num == "111" = "7"
    | otherwise = "0"

searchHexa :: String -> String
searchHexa [] = ""
searchHexa num
    | num == "0001" = "1"
    | num == "0010" = "2"
    | num == "0011" = "3"
    | num == "0100" = "4"
    | num == "0101" = "5"
    | num == "0110" = "6"
    | num == "0111" = "7"
    | num == "1000" = "8"
    | num == "1001" = "9"
    | num == "1010" = "A"
    | num == "1011" = "B"
    | num == "1100" = "C"
    | num == "1101" = "D"
    | num == "1110" = "E"
    | num == "1111" = "F"
    | otherwise = "0"

--Funcion para obtener una lista de potencias de 2
getPow2 num = reverse (1 : [2^x|x<-[1,2..num-1]])

--Funcion que busca un multiplo de 3 mayor al numero dado
searchMultiple3 :: Int -> Int
searchMultiple3 num 
    | num `mod` 3 ==0 = num
    | otherwise = head [x| x<- [3,6..54], x>num]

--Funcion que busca un multiplo de 4 mayor al numero dado
searchMultiple4 :: Int -> Int
searchMultiple4 num 
    | num `mod` 4 ==0 = num
    | otherwise = head [x| x<- [4,8..100], x>num]