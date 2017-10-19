import Data.Char

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
fibDigits = map (length . show) fibs
