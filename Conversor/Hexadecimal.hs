module Conversor.Hexadecimal
( hexadecimalBinario
, hexadecimalOctal
, hexadecimalDecimal) where

import Conversor.Binario
import Conversor.Funciones
import Conversor.FuncionesHexa
--Funciones Hexadecimales
hexadecimalBinario :: String -> String
hexadecimalBinario "0" = "0000"
hexadecimalBinario num = transformHexaBin(split num)

hexadecimalOctal :: String -> String
hexadecimalOctal "0" = "0"
hexadecimalOctal num = binarioOctal (hexadecimalBinario num)

hexadecimalDecimal :: String -> Int
hexadecimalDecimal num = rest
    where
        list1 = transformInt(transformHexa (split num))
        list2 = getPow16 (length num)
        rest = converter list1 list2