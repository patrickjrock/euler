import Math.NumberTheory.GCD
import Math.NumberTheory.Primes
import Data.List

type Frac = (Int,Int)

resilient :: Frac -> Bool
resilient (x,y) = coprime x y


resilience' n = fromIntegral (length $ filter id res) / fromIntegral (length res) 
  where cs = zip [1..(n-1)] (repeat n)
        res = map resilient cs


resilience n = (fromIntegral (totient n)) / (fromIntegral (n-1))

totient n = sieveTotient ts n
  where ts = totientSieve n

nthProd n = product $ take n primes

nthToti n = round $ p * (product $ f <$> take n primes)
  where f x = 1 - (1/fromIntegral x)
        p = fromIntegral $ product $ take n primes

nthRes n = fromIntegral (nthToti n) / fromIntegral (nthProd n - 1)


factorToti l = round $ p * (product $ f <$> l')
  where f x = 1 - (1/fromIntegral x)
        p = fromIntegral $ product l
        l' = nub l

factorRes l = fromIntegral (factorToti l) / fromIntegral (product l -1)

candidate l = (factorRes l) < (15499/94744)

l = [2,2] ++  take 9 primes

main = do print l 
