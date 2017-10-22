import Data.Char

digitize x = map digitToInt $ show x 

sumOfPowers n = n == (sum $ map (^5) digits)
  where digits = digitize n 
