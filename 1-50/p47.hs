import Data.Numbers.Primes
import Data.List

distinctFactors n a b = (disjoint pa pb) && (la==n) && (lb==n)
  where disjoint x y = ((intersect x y) == [])  
        pa = nub $ primeFactors a
        pb = nub $ primeFactors b
        la = length pa
        lb = length pb

candidate = distinctFactors 4

consec a = and [p1,p2,p3]
  where p1 = distinctFactors 4 a (a+1)
        p2 = distinctFactors 4 (a+1) (a+2)
        p3 = distinctFactors 4 (a+2) (a+3)

main = do print $ head $ filter (\(a,b)->a) $ zip (map consec [1..]) [1..]
