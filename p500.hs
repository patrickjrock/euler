import Math.NumberTheory.Primes
import Data.List 
import Data.Function (on)
import Data.Ord

powDiv 1 = 2
powDiv 2 = 6
powDiv 3 = 24
powDiv n = 2*3*4*p
  where p = foldl f 1 $ take (n-3) primes
        odds = [5,7..]
        f a b = mod (a*b) 500500507


-- return factorization of number with next power of two divisors
--nextPower :: [(Integer,Integer)] -> [(Integer,Integer)]
nextPower l = 
  if nextPrime < (snd minDoub) 
  then l ++ [(nextPrime, 1)]
  else l' ++ [(fst minDoub, 2*(snd elem)+1)]
  where doub (a,b) = (a,a^(b+1))
        doubles = map doub l
        minDoub = minimumBy (comparing snd) doubles 
        nextPrime = head $ primes \\ (map fst l)
        elem = head $ filter (\(a,b)->a == (fst minDoub)) l
        l' = delete elem l
