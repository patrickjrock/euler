import Data.List

factors :: Int -> [Int]
factors 1 = [1]
factors n = pairs ++ (map (div n) pairs) ++ [1] 
  where r = floor $ sqrt $ fromIntegral n
        pairs = filter (\x -> mod n x == 0) [2..r]

amicablePair (a, b) = if d a == b && d b == a && a /= b then True else False
  where d = sum . factors

getPair :: Int -> (Int,Int)
getPair n =  if amicablePair (n, s) then (n,s) else (0,0)
  where s = sum $ factors n 

amicable n = if amicablePair (n, s) then True else False
  where s = sum $ factors n 




main = do print $ sum $ filter amicable [1..10000]
