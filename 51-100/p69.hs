import Data.Numbers.Primes
import Data.List

term :: Integer -> Double
term p = 1 - (1/ (fromIntegral p))

toitent :: Integer -> Integer
toitent n =  round $ (fromIntegral n) * ( product $ map term pfs)
  where pfs = nub $ primeFactors n


nphi n = (fromIntegral n) / (fromIntegral (toitent n))
