import Data.Char
import Data.Numbers.Primes

digitize x = map digitToInt $ show x 
unDigitize x = sum $ reverse $ zipWith (*) (reverse x) [10^n | n <- [0..]]

rotations' _ 0 = []
rotations' (x:xs) n = (r) : (rotations' r (n-1))
  where r = xs ++ [x]
rotations n = map unDigitize $ rotations' digits (length digits)
  where digits = digitize n

circularPrime n = and $ map isPrime (rotations n)
circularPrimes = filter circularPrime (takeWhile (<1000000) primes)

