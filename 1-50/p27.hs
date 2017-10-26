import Data.Function
import Data.List 
import Data.Ord

coFactors :: Int -> [Int]
coFactors n = filter (test n) [1..(intSqrt n)] --generates factors upto sqrt
  where intSqrt = floor . sqrt . fromIntegral
        test n x = mod n x == 0

factors :: Int -> [Int]
factors n = (fmap (div n) fac) ++ fac  
  where fac = coFactors n

prime n = (length $ factors n) <= 2 && n > 0

quad (a,b) = (\n -> n*n + n*a + b)
params n = [(a,b) | a <- [-n..n],
                    b <- [-n..n],
                    odd b]

quads n = map quad (params n) 

countPrimes f = length $ takeWhile prime ys
  where ys = map f [0..]

countAll n = maximumBy (comparing fst) $ zip (map countPrimes $ quads n) (params n) 

main = do print $ countAll 1000
