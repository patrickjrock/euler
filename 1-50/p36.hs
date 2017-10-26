import Data.Char
import Data.Digits (digits, unDigits)

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from
toBinary = convertBase 10 2

digitize x = map digitToInt $ show x 

palindrome s = s == reverse s

doublePal n = (palindrome $ digitize n) && (palindrome $ toBinary [n])
