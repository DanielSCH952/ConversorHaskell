module Conversor.FuncionesDecimales
( divHex
, transformNumHex
, decimalBinario
, divBin
, transformNumBin
, descomponerNumero
, convUni
, convDec
, convCent
, convUnM
, convDecM
, convCentM
, convMillon
, convDecMillon
, convCentMillon) where


import Data.Char (digitToInt)

divHex :: Int-> String
divHex 0 = ""
divHex num = let residuo = num `mod` 16
                 cociente = num `div` 16
                 in transformNumHex residuo ++ divHex cociente

transformNumHex :: Int-> String
transformNumHex num 
    | num ==10 = "A"
    | num ==11 = "B"
    | num ==12 = "C"
    | num ==13 = "D"
    | num ==14 = "E"
    | num ==15 = "F"
    | otherwise = show num

decimalBinario:: Int->String
decimalBinario 0 = "0"
decimalBinario num = transformNumBin (reverse(divBin num)) num

divBin :: Int -> String
divBin 0 = ""
divBin num = let (cociente,residuo) = num `divMod` 2 
             in show residuo ++ divBin cociente

transformNumBin :: String -> Int -> String
transformNumBin bin num
    | (length bin < 9) && (num <256) = replicate (8-(length bin)) '0' ++ bin
    | (length bin < 13) && (num <4096) = replicate (12-(length bin))  '0' ++ bin
    | (length bin < 17) && (num <65536) = replicate (16-(length bin)) '0' ++ bin
    | otherwise = bin ++" Su numero binario tiene o rebasa los 17 caracteres, numeros mayores a 65535 son muy grandes"

descomponerNumero :: Int -> (Int, Int,Int,Int, Int,Int, Int, Int, Int)
descomponerNumero numero = (decenaMil *10000,millares * 1000, centenas * 100 , decenas *10, unidades *1)
  where
    numeroStr = show numero  -- Convertir el número en una cadena
    padding = replicate (9 - length numeroStr) '0' ++ numeroStr
    [centenaMillon,decenaMillon,millon,centenaMil, decenaMil, millares, centenas, decenas, unidades] = map digitToInt padding

convUni :: Int -> String
convUni 0 = "0"
convUni num 
    | flag1 = replicate num 'I'
    | flag2 = "V"
    | flag3 = "V" ++replicate (num-5) 'I'
    | otherwise = if(num == 4) then "IV" else "IX"
        where
            flag1 = num < 4
            flag2 = num ==5
            flag3 = num > 5 && num <9

convDec :: Int -> String
convDec 0 = ""
convDec num 
    | flag1 = replicate (num `div`10 )'X'
    | flag2 = "L"
    | flag3 = "L" ++replicate ((num-50)`div`10) 'X'
    | otherwise = if(num == 40) then "XL" else "XC"
        where
            flag1 = num < 40
            flag2 = num ==50
            flag3 = num > 50 && num < 90

convCent :: Int -> String
convCent 0 = ""
convCent num 
    | flag1 = replicate (num`div`100) 'C'
    | flag2 = "D"
    | flag3 = "D" ++replicate ((num-500)`div`100) 'C'
    | otherwise = if(num == 400) then "CD" else "CM"
        where
            flag1 = num < 400
            flag2 = num ==500
            flag3 = num > 500 && num < 900

convUnM :: Int -> String
convUnM 0 = ""
convUnM num 
    | flag1 = replicate (num`div`1000) 'M'
    | flag2 = "|V|"
    | flag3 = "|V" ++replicate ((num-5000)`div`1000) 'I' ++"|"
    | otherwise = if(num == 4000) then "|IV|" else "|IX|"
        where
            flag1 = num < 4000
            flag2 = num ==5000
            flag3 = num > 5000 && num < 9000

convDecM :: Int -> String
convDecM 0 = ""
convDecM num 
    | flag1 = "|"++ replicate (num`div`10000) 'X' ++ "|"
    | flag2 = "|L|"
    | flag3 = "|L" ++replicate ((num-50000)`div`10000) 'X' ++"|"
    | otherwise = if(num == 40000) then "|XL|" else "|XC|"
        where
            flag1 = num < 40000
            flag2 = num ==50000
            flag3 = num > 50000 && num < 90000

convCentM :: Int -> String
convCentM 0 = ""
convCentM num 
    | flag1 = "|"++ replicate (num`div`100000) 'C' ++ "|"
    | flag2 = "|D|"
    | flag3 = "|D" ++replicate ((num-500000)`div`100000) 'C' ++"|"
    | otherwise = if(num == 40000) then "|CD|" else "|CM|"
        where
            flag1 = num < 400000
            flag2 = num ==500000
            flag3 = num > 500000 && num < 900000

convMillon :: Int -> String
convMillon 0 = ""
convMillon num 
    | flag1 = if(num == 1000000) then "||M||" else "||"++ replicate (num`div`1000000) 'I' ++ "||"
    | flag2 = "||V||"
    | flag3 = "||V" ++replicate ((num-5000000)`div`1000000) 'I' ++"||"
    | otherwise = if(num == 4000000) then "||IV||" else "||IX||"
        where
            flag1 = num < 4000000
            flag2 = num ==5000000
            flag3 = num > 5000000 && num < 9000000

convDecMillon :: Int -> String
convDecMillon 0 = ""
convDecMillon num 
    | flag1 = "||"++ replicate (num`div`10000000) 'X' ++ "||"
    | flag2 = "||L||"
    | flag3 = "||L" ++replicate ((num-50000000)`div`10000000) 'X' ++"||"
    | otherwise = if(num == 40000000) then "||XL||" else "||XC||"
        where
            flag1 = num < 40000000
            flag2 = num ==50000000
            flag3 = num > 50000000 && num < 90000000

convCentMillon :: Int -> String
convCentMillon 0 = ""
convCentMillon num 
    | flag1 = "||"++ replicate (num`div`100000000) 'X' ++ "||"
    | flag2 = "||L||"
    | flag3 = "||L" ++replicate ((num-500000000)`div`100000000) 'X' ++"||"
    | otherwise = if(num == 400000000) then "||XL||" else "||XC||"
        where
            flag1 = num < 400000000
            flag2 = num ==500000000
            flag3 = num > 500000000 && num < 900000000