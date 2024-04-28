import Conversor.Decimal
import Conversor.Hexadecimal
import Conversor.Octal
import Conversor.Binario
--Decimal
decHexa num = decimalHexadecimal num
decBin num = decimalBinario num
decOct num = decimalOctal num
decRom num = decimalRomano num
--Hexadecimal
hexaOct num = hexadecimalOctal num
hexaBin num = hexadecimalBinario num
hexaDec num = hexadecimalDecimal num

--Octal
octHexa num = octalHexadecimal num
octBin num = octalBinario num
octDec num = octalDecimal num

--Binario
binHexa num = binarioHexadecimal num
binOct num = binarioOctal num
binDec num = binarioDecimal num