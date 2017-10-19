hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs


divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

abundant :: Int -> Bool
abundant 0 = False
abundant n = s > n 
  where s = sum $ divisors n 

abundants n = filter abundant [1..n]

--sumOfAbundant :: Int -> Bool
sumOfAbundant n = or $ map abundant ds 
  where ds = map abs $ map ((+) (-1*n)) $ abundants n 

main = do print $ sum $ filter (not . sumOfAbundant) [1..28124]
