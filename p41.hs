import Data.Char
import Data.List
import Data.Numbers.Primes

digitize x = map digitToInt $ show x 

pandigital n a = (null ([1..n] \\ x)) && (length x == n)
  where x = digitize a

nPan a = or $ zipWith ($) (map pandigital [1..9]) (repeat a)

panPrimes = filter nPan primes
