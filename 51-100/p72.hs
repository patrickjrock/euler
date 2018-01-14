import Data.List
import Data.Numbers.Primes

totient :: Int -> Int
totient 1 = 1
totient a =  round $ (fromIntegral a) * (product $ f <$> pfs)
 where pfs = nub $ primeFactors a 
       f x = (1-(1/(fromIntegral x)))


main = do print $ sum $ map totient [2..1000000]
