module Conversor.Funciones
( split
, converter
, transformInt) where

--Funcion para partir una lista en subelementos
split :: String -> [[Char]]
split cadena = map (\c -> [c]) cadena

converter :: [Int] -> [Int] -> Int
converter [] [] = 0
converter (x:restx) (y:resty) = (x*y) + converter (restx) (resty)

--Para convertir una lista de caracteres en una de numeros
transformInt xs = map (\c -> read c -0) xs
