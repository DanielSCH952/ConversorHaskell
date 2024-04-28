module Conversor.Octal
( octalBinario
, octalHexadecimal
, octalDecimal) where

import Conversor.FuncionesOctales
import Conversor.Binario
import Conversor.Funciones
--Funciones Octales
octalBinario :: String -> String
octalBinario "0" = "000"
octalBinario num = bin
    where
        bin=  transformOctBin (transformInt (split num))

octalHexadecimal :: String -> String
octalHexadecimal num = binarioHexadecimal (octalBinario num)

octalDecimal ::String ->Int
octalDecimal num = rest
    where
        list1 = transformInt(split num)
        list2 = getPow8 (length num)
        rest = converter list1 list2