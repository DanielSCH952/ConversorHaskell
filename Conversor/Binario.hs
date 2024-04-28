module Conversor.Binario
( binarioOctal
, binarioHexadecimal
, binarioDecimal)where

import Conversor.FuncionesBinarios
import Conversor.Funciones
--Funciones Binarias
--Funcion de Binario a Octal
binarioOctal :: String -> String
binarioOctal "0" = "0"
binarioOctal num = resultado
    where
        numComplete = completeForOctal num
        list = getlista3 numComplete
        resultado = concat(map searchOctal list)

--Funcion de Binario a Hexadecimal
binarioHexadecimal :: String -> String
binarioHexadecimal "0" = "0"
binarioHexadecimal num = resultado
    where
        numComplete = completeForHexa num
        list = getlista4 numComplete
        resultado = concat(map searchHexa list)

--Funcion de Binario a decimal
binarioDecimal :: String -> Int
binarioDecimal num = rest
    where 
        list1 = transformInt(split num)
        list2 = getPow2 (length num)
        rest = converter list1 list2