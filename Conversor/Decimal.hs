module Conversor.Decimal
( decimalOctal
, decimalHexadecimal
, decimalBinario
, decimalRomano
)where

import Conversor.FuncionesDecimales

decimalOctal:: Int-> String
decimalOctal 0  = "0"
decimalOctal num = reverse (divOctal num)

divOctal :: Int -> String
divOctal 0 = ""
divOctal num = let residuo = num `mod` 8
                   cociente = num `div` 8
                in show residuo ++ divOctal cociente

decimalHexadecimal :: Int->String
decimalHexadecimal 0 = "0"
decimalHexadecimal num = reverse (divHex num)

--(0,10000000,0,500000,40000,0,700,20,0)
decimalRomano :: Int -> String
decimalRomano num = resultado 
    where
        (cMll,dMll,uMll,cM,dM,uM,c,d,u) = descomponerNumero num
        unidades = if(convUni u == "0") then "" else convUni u
        decenas =  if(convDec d == "0") then "" else convDec d
        centenas = if(convCent c == "0") then "" else convCent c
        uniMillar = if(convUnM uM == "0") then "" else convUnM uM
        decMillar = if(convDecM dM == "0") then "" else convDecM dM
        cenMillar = if(convCentM cM == "0") then "" else convCentM cM
        uniMillon = if(convMillon uMll == "0") then "" else convMillon uMll
        decMillon = if(convDecMillon dMll == "0") then "" else convDecMillon dMll
        cenMillon = if(convCentMillon cMll == "0") then "" else convCentMillon cMll
        resultado = cenMillon++decMillon++uniMillon++cenMillar++decMillar++uniMillar++centenas++decenas++unidades