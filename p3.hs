import Data.List

coFactors :: Int -> [Int]
coFactors n = filter (test n) [1..(intSqrt n)] --generates factors upto sqrt
  where intSqrt = floor . sqrt . fromIntegral
        test n x = mod n x == 0

factors :: Int -> [Int]
factors n = (fmap (div n) fac) ++ fac  
  where fac = coFactors n

prime n = (length $ factors n) <= 2

largestPrime' :: Int -> [Int]
largestPrime' n = dropWhile (not . prime) (reverse $ sort $ factors n)

largestPrime :: Int -> Int
largestPrime n = head $ largestPrime' n 

main = do print $ largestPrime 600851475143
