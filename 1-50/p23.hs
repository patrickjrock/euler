import Data.List

divisors n = (1:) $ nub $ concat [ [x, div n x] | x <- [2..limit], rem n x == 0 ]
     where limit = (floor.sqrt.fromIntegral) n


abundant :: Int -> Bool
abundant 0 = False
abundant n = s > n 
  where s = sum $ divisors n 

abundants = filter abundant [1..]
abundant' = map abundant [1..]
fastAbundant n = abundant' !! (n-1)

sumOfAbundant :: Int -> Bool
sumOfAbundant n = any fastAbundant [n-ab | ab <- takeWhile (<n) abundants]

p23 n = sum $ filter (not . sumOfAbundant) [1..n]

main = do print $ sum $ filter (not . sumOfAbundant) [1..20161]
