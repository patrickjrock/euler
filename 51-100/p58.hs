import Data.Numbers.Primes
import Data.List


diagonals = 1 : zipWith (+) (map f [1..]) diagonals 
  where f n = (div (n-1) 4)*2 +2

primeagonals = map ((\b->if b then 1 else 0) . isPrime) diagonals

squareRatio x = s / (fromIntegral n) 
  where s = fromIntegral $ sum $ take n primeagonals
        n = (2*(x-1) + 1)

main = do print $ head $ dropWhile (\(a,b)->b>0.1) $ zip [3,5..] $  map squareRatio [3,5..]
