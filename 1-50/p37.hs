import Data.Numbers.Primes
import Data.Char
import Data.Numbers.Primes
import Data.List

digitize x = map digitToInt $ show x 
unDigitize x = sum $ reverse $ zipWith (*) (reverse x) [10^n | n <- [0..]]

truncatable' f n = and $ map isPrime ts 
  where ts = map unDigitize $ f $ digitize n 

truncatable n = (truncatable' (tail . inits) n) && (truncatable' (init . tails) n)
